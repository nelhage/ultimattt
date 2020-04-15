use crate::game;
use crate::prove;
use crate::table;
use std::cmp::{max, min};
use std::time::{Duration, Instant};
use typenum;

#[derive(Clone, Debug, Default)]
pub struct Stats {
    pub mid: usize,
    pub ttlookup: usize,
    pub tthit: usize,
    pub ttstore: usize,
    pub terminal: usize,
}

const INFINITY: u32 = 1 << 31;

#[derive(Copy, Clone, Debug)]
pub struct Bounds {
    pub phi: u32,
    pub delta: u32,
}

impl Bounds {
    fn winning() -> Self {
        Bounds {
            phi: 0,
            delta: INFINITY,
        }
    }

    fn losing() -> Self {
        Bounds {
            phi: INFINITY,
            delta: 0,
        }
    }

    fn unity() -> Self {
        Bounds { phi: 1, delta: 1 }
    }

    fn infinity() -> Self {
        Bounds {
            phi: INFINITY,
            delta: INFINITY,
        }
    }
}

#[derive(Clone)]
struct Entry {
    bounds: Bounds,
    hash: u64,
    work: u64,
    pv: game::Move,
}

impl table::Entry for Entry {
    fn hash(&self) -> u64 {
        self.hash
    }

    fn better_than(&self, other: &Entry) -> bool {
        self.work > other.work
    }

    fn valid(&self) -> bool {
        self.work != std::u64::MAX
    }
}

impl Default for Entry {
    fn default() -> Self {
        Entry {
            bounds: Bounds { phi: 0, delta: 0 },
            hash: 0,
            work: std::u64::MAX,
            pv: game::Move::none(),
        }
    }
}

#[derive(Clone)]
pub struct Config {
    pub table_size: usize,
    pub timeout: Option<Duration>,
    pub debug: usize,
    pub epsilon: f64,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            table_size: table::DEFAULT_TABLE_SIZE,
            timeout: None,
            debug: 0,
            epsilon: 1.0 / 8.0,
        }
    }
}

pub struct ProveResult {
    pub value: prove::Evaluation,
    pub bounds: Bounds,
    pub stats: Stats,
    pub duration: Duration,
    pub work: u64,
    pub pv: Vec<game::Move>,
}

pub struct DFPN {
    start: Instant,
    tick: Instant,
    cfg: Config,
    table: table::TranspositionTable<Entry, typenum::U4>,
    stats: Stats,
    root: game::Game,
    stack: Vec<game::Move>,
}

struct Child {
    position: game::Game,
    r#move: game::Move,
    entry: Entry,
}

const CHECK_TICK_INTERVAL: usize = 1 << 12;
static TICK_TIME: Duration = Duration::from_millis(500);

impl DFPN {
    pub fn prove(cfg: &Config, g: &game::Game) -> ProveResult {
        let start = Instant::now();
        let mut prover = DFPN {
            root: g.clone(),
            start: start,
            tick: start,
            cfg: cfg.clone(),
            table: table::TranspositionTable::with_memory(cfg.table_size),
            stats: Default::default(),
            stack: Vec::new(),
        };
        let (result, work) = prover.mid(
            Bounds {
                phi: INFINITY / 2,
                delta: INFINITY / 2,
            },
            Entry {
                bounds: Bounds::unity(),
                hash: g.zobrist(),
                work: 0,
                pv: game::Move::none(),
            },
            g,
        );
        ProveResult {
            value: if result.bounds.phi == 0 {
                prove::Evaluation::True
            } else if result.bounds.delta == 0 {
                prove::Evaluation::False
            } else {
                prove::Evaluation::Unknown
            },
            work: work,
            bounds: result.bounds,
            stats: prover.stats.clone(),
            duration: Instant::now().duration_since(prover.start),
            pv: prover.extract_pv(),
        }
    }

    fn extract_pv(&self) -> Vec<game::Move> {
        let mut pv = Vec::new();
        let mut g = self.root.clone();
        loop {
            match g.game_state() {
                game::BoardState::InPlay => (),
                _ => break,
            };
            if let Some(ent) = self.table.lookup(g.zobrist()) {
                if ent.bounds.phi != 0 && ent.bounds.delta != 0 {
                    assert!(
                        pv.len() == 0,
                        "PV contains unproven positions d={} bounds=({}, {}) work={}",
                        pv.len(),
                        ent.bounds.phi,
                        ent.bounds.delta,
                        ent.work,
                    );
                    break;
                }
                pv.push(ent.pv);
                g = g.make_move(ent.pv).unwrap_or_else(|_| {
                    panic!("PV contained illegal move depth={} m={}", pv.len(), ent.pv)
                });
            } else {
                break;
            }
        }
        pv
    }

    fn mid(&mut self, bounds: Bounds, mut data: Entry, pos: &game::Game) -> (Entry, u64) {
        if self.cfg.debug > 4 {
            eprintln!(
                "mid[{}]: m={} d={} bounds=({}, {})",
                self.stack
                    .last()
                    .map(|&m| game::notation::render_move(m))
                    .unwrap_or_else(|| "<root>".to_owned()),
                self.stats.mid,
                self.stack.len(),
                bounds.phi,
                bounds.delta
            );
        }
        self.stats.mid += 1;
        if self.stats.mid % CHECK_TICK_INTERVAL == 0
            && self.cfg.debug > 0
            && Instant::now() > self.tick
        {
            self.tick += TICK_TIME;
            let elapsed = Instant::now().duration_since(self.start);
            eprintln!(
                "t={}.{:03}s mid={} mid/ms={:.1}",
                elapsed.as_secs(),
                elapsed.subsec_millis(),
                self.stats.mid,
                (self.stats.mid as f64) / (elapsed.as_millis() as f64),
            );
        }
        if (data.bounds.phi >= bounds.phi) || (data.bounds.delta >= bounds.delta) {
            return (data, 0);
        }
        let terminal = match pos.game_state() {
            game::BoardState::InPlay => None,
            game::BoardState::Drawn => Some(false),
            game::BoardState::Won(p) => Some(p == pos.player()),
        };
        if let Some(v) = terminal {
            if v {
                data.bounds = Bounds::winning();
            } else {
                data.bounds = Bounds::losing();
            }
            self.stats.terminal += 1;
            data.work = 1;
            if self.table.store(&data) {
                self.stats.ttstore += 1;
            }
            return (data, 1);
        }

        let mut local_work = 1;

        let mut children = Vec::new();
        for m in pos.all_moves() {
            let g = pos.make_move(m).expect("all_moves returned illegal move");
            self.stats.ttlookup += 1;
            let te = self.table.lookup(g.zobrist()).cloned();
            if let Some(_) = te {
                self.stats.tthit += 1;
            }
            let data = te.unwrap_or(Entry {
                bounds: Bounds::unity(),
                hash: g.zobrist(),
                work: 0,
                pv: game::Move::none(),
            });
            let state = g.game_state();
            children.push(Child {
                position: g,
                r#move: m,
                entry: data,
            });
            if let game::BoardState::Won(p) = state {
                if p == pos.player() {
                    break;
                }
            }
        }

        // recurse

        loop {
            data.bounds = self.compute_bounds(&children);
            if data.bounds.phi >= bounds.phi || data.bounds.delta >= bounds.delta {
                break;
            }
            let (best_idx, delta_2) = self.select_child(&children);
            let child = &children[best_idx];
            let child_bounds = Bounds {
                phi: bounds.delta + child.entry.bounds.phi - data.bounds.delta,
                delta: min(
                    bounds.phi,
                    max(
                        delta_2 + 1,
                        (delta_2 as f64 * (1.0 + self.cfg.epsilon)) as u32,
                    ),
                ),
            };
            self.stack.push(children[best_idx].r#move);
            let (child_entry, child_work) = self.mid(
                child_bounds,
                children[best_idx].entry.clone(),
                &child.position,
            );
            children[best_idx].entry = child_entry;
            self.stack.pop();
            local_work += child_work;
        }

        data.work += local_work;
        // If the position is solved, store the PV. For winning moves,
        // find the shortest path to victory; for losing, the
        // most-delaying
        if data.bounds.phi == 0 {
            data.pv = children
                .iter()
                .filter(|e| e.entry.bounds.delta == 0)
                .min_by_key(|e| e.entry.work)
                .map(|e| e.r#move)
                .unwrap_or(game::Move::none());
        } else if data.bounds.delta == 0 {
            data.pv = children
                .iter()
                .max_by_key(|e| e.entry.work)
                .map(|e| e.r#move)
                .unwrap_or(game::Move::none());
        }
        if self.table.store(&data) {
            self.stats.ttstore += 1;
        }
        (data, local_work)
    }

    fn compute_bounds(&self, children: &Vec<Child>) -> Bounds {
        let mut out = Bounds {
            phi: INFINITY,
            delta: 0,
        };
        for ch in children.iter() {
            out.phi = min(out.phi, ch.entry.bounds.delta);
            out.delta = out.delta.saturating_add(ch.entry.bounds.phi);
        }
        out.delta = min(out.delta, INFINITY);
        return out;
    }

    // returns (child index, delta_2)
    fn select_child(&self, children: &Vec<Child>) -> (usize, u32) {
        let (mut delta_1, mut delta_2) = (INFINITY, INFINITY);
        let mut idx = children.len();
        for (i, ch) in children.iter().enumerate() {
            if ch.entry.bounds.delta < delta_1 {
                delta_2 = delta_1;
                delta_1 = ch.entry.bounds.delta;
                idx = i;
            } else if ch.entry.bounds.delta < delta_2 {
                delta_2 = ch.entry.bounds.delta;
            }
        }
        (idx, delta_2)
    }
}
