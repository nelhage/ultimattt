use crate::endgame;
use crate::game;
use crate::minimax;
use crate::progress::Ticker;
use crate::prove;
use crate::prove::spdfpn;
use crate::prove::{Bounds, INFINITY};
use crate::table;
use crate::util;

use hdrhistogram::Histogram;
use serde::Serialize;

use std::cmp::{max, min};
use std::io::Write;
use std::sync::atomic::AtomicU32;
use std::time::{Duration, Instant};
use std::{fs, io, mem};

#[derive(Clone, Debug, Serialize)]
pub struct Stats {
    pub mid: usize,
    pub try_calls: usize,
    pub jobs: usize,
    pub solved: usize,
    pub minimax: usize,
    pub minimax_solve: usize,
    pub endgame_solve: usize,
    pub endgame_move: usize,
    #[serde(serialize_with = "util::serialize_histogram")]
    pub branch: Histogram<u64>,
    #[serde(serialize_with = "util::serialize_histogram")]
    pub open_boards: Histogram<u64>,
    #[serde(flatten)]
    pub tt: table::Stats,
    pub endgame: endgame::Stats,
}

impl Default for Stats {
    fn default() -> Self {
        Stats {
            mid: 0,
            try_calls: 0,
            jobs: 0,
            solved: 0,
            minimax: 0,
            minimax_solve: 0,
            endgame_solve: 0,
            endgame_move: 0,
            branch: Histogram::new_with_max(81, 3).unwrap(),
            open_boards: Histogram::new_with_max(9, 2).unwrap(),
            tt: Default::default(),
            endgame: Default::default(),
        }
    }
}

impl Stats {
    pub fn merge(&self, other: &Stats) -> Stats {
        Stats {
            mid: self.mid + other.mid,
            jobs: self.jobs + other.jobs,
            try_calls: self.try_calls + other.try_calls,
            solved: self.solved + other.solved,
            minimax: self.minimax + other.minimax,
            minimax_solve: self.minimax_solve + other.minimax_solve,
            endgame_solve: self.endgame_solve + other.endgame_solve,
            endgame_move: self.endgame_move + other.endgame_move,
            branch: util::merge_histogram(&self.branch, &other.branch),
            open_boards: util::merge_histogram(&self.open_boards, &other.open_boards),
            tt: self.tt.merge(&other.tt),
            endgame: self.endgame.merge(&other.endgame),
        }
    }
}

#[repr(C)]
pub(in crate::prove) struct Entry {
    pub(in crate::prove) bounds: Bounds,  // 8
    pub(in crate::prove) hash: u64,       // 8
    pub(in crate::prove) work: u64,       // 8
    pub(in crate::prove) sync: AtomicU32, // 4
    pub(in crate::prove) pv: game::Move,  // 1
    pub(in crate::prove) child: u8,       // 1
}

pub fn sizeof_entry() -> usize {
    mem::size_of::<Entry>()
}

impl Clone for Entry {
    fn clone(&self) -> Self {
        Entry {
            bounds: self.bounds.clone(),
            hash: self.hash,
            work: self.work,
            sync: AtomicU32::new(0),
            pv: self.pv,
            child: self.child,
        }
    }
}

impl table::Entry for Entry {
    fn hash(&self) -> u64 {
        self.hash
    }

    fn better_than(&self, other: &Entry) -> bool {
        if self.hash == other.hash {
            if self.bounds.solved() != other.bounds.solved() {
                return self.bounds.solved();
            }
        }
        self.work >= other.work
    }

    fn valid(&self) -> bool {
        self.work != std::u64::MAX
    }
}

impl table::AtomicEntry for Entry {
    unsafe fn lock(e: *const Self) -> *const AtomicU32 {
        &(*e).sync
    }

    unsafe fn write(dst: *mut Self, v: &Self) {
        (*dst).bounds = v.bounds;
        (*dst).hash = v.hash;
        (*dst).work = v.work;
        (*dst).pv = v.pv;
        (*dst).child = v.child;
    }
}

impl Default for Entry {
    fn default() -> Self {
        Entry {
            bounds: Bounds { phi: 0, delta: 0 },
            hash: 0,
            work: std::u64::MAX,
            pv: game::Move::none(),
            child: std::u8::MAX,
            sync: AtomicU32::new(0),
        }
    }
}

#[derive(Clone)]
pub struct Config {
    pub threads: usize,
    pub table_size: usize,
    pub timeout: Option<Duration>,
    pub debug: usize,
    pub epsilon: f64,
    pub max_work_per_job: u64,
    pub dump_table: Option<String>,
    pub load_table: Option<String>,
    pub dump_interval: Duration,
    pub minimax_cutoff: usize,
    pub probe_hash: Option<u64>,
    pub probe_log: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            threads: 0,
            table_size: table::DEFAULT_TABLE_SIZE,
            timeout: None,
            debug: 0,
            epsilon: 1.0 / 8.0,
            max_work_per_job: 500,
            dump_table: None,
            load_table: None,
            dump_interval: Duration::from_secs(30),
            minimax_cutoff: 0,
            probe_hash: None,
            probe_log: "probe.csv".to_owned(),
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
    cfg: Config,
    stats: Stats,
    root: game::Game,
}

#[derive(Clone)]
pub(in crate::prove) struct Child {
    pub(in crate::prove) position: game::Game,
    pub(in crate::prove) r#move: game::Move,
    pub(in crate::prove) entry: Entry,
}

const CHECK_TICK_WORK: u64 = 500000;
pub static TICK_TIME: Duration = Duration::from_millis(500);

impl DFPN {
    pub fn prove(cfg: &Config, g: &game::Game) -> ProveResult {
        let start = Instant::now();
        let mut prover = DFPN {
            root: g.clone(),
            start: start,
            cfg: cfg.clone(),
            stats: Default::default(),
        };
        let (result, pv, work) = prover.run();
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
            pv: pv,
        }
    }

    fn run(&mut self) -> (Entry, Vec<game::Move>, u64) {
        let mut probe = Probe::from_config(&self.cfg);

        let mmcfg = minimax::Config {
            max_depth: Some(self.cfg.minimax_cutoff as i64 + 1),
            timeout: Some(Duration::from_secs(1)),
            debug: if self.cfg.debug > 6 { 1 } else { 0 },
            table_bytes: None,
            draw_winner: Some(self.root.player().other()),
        };

        if self.cfg.threads == 0 {
            let table = if let Some(ref path) = self.cfg.load_table {
                table::TranspositionTable::<_, 4>::from_file(path).expect("invalid table file")
            } else {
                table::TranspositionTable::<_, 4>::with_memory(self.cfg.table_size)
            };
            let mut worker = MID {
                id: 0,
                cfg: &self.cfg,
                player: self.root.player(),
                table: table,
                stats: Default::default(),
                stack: Vec::new(),
                minimax: minimax::Minimax::with_config(&mmcfg),
                probe: |stats: &Stats, pos: &game::Game, data: &Entry, children: &Vec<Child>| {
                    if let Some(ref mut p) = probe {
                        if data.hash != p.hash {
                            return;
                        }
                        let now = Instant::now();
                        if now < p.tick && !data.bounds.solved() {
                            return;
                        }
                        p.do_probe(
                            now + Duration::from_millis(10),
                            stats.mid,
                            pos,
                            data,
                            children,
                        );
                    }
                },
            };
            let mut root = Entry {
                hash: self.root.zobrist(),
                bounds: Bounds::unity(),
                child: 0xff,
                pv: game::Move::none(),
                work: 0,
                sync: AtomicU32::new(0),
            };
            let mut work = 0;
            let mut dump_tick = Ticker::new(self.cfg.dump_interval);
            while !root.bounds.solved() {
                let (out, this_work, _) = worker.mid(
                    Bounds {
                        phi: INFINITY / 2,
                        delta: INFINITY / 2,
                    },
                    CHECK_TICK_WORK,
                    root,
                    &self.root,
                );
                root = out;
                work += this_work;
                let now = Instant::now();
                if self.cfg.debug > 0 {
                    let elapsed = now - self.start;
                    eprintln!(
                        "t={}.{:03}s mid={} root={:?}",
                        elapsed.as_secs(),
                        elapsed.subsec_millis(),
                        worker.stats.mid,
                        root.bounds,
                    );
                }

                if let Some(_) = self.cfg.dump_table {
                    if dump_tick.tick() {
                        dump_table(&self.cfg, &worker.table).expect("dump_table failed");
                    }
                }
            }
            self.stats = worker.stats.clone();
            let pv = extract_pv(&worker.cfg, &mut worker.table, &self.root);
            self.stats.tt = worker.table.stats();
            dump_table(&self.cfg, &worker.table).expect("final dump_table");
            (root, pv, work)
        } else {
            let (stats, root, pv, work) = spdfpn::run(self.start, &self.cfg, &self.root, probe);
            self.stats = stats;
            (root, pv, work)
        }
    }
}

pub(in crate::prove) struct Probe {
    pub(in crate::prove) tick: Instant,
    pub(in crate::prove) hash: u64,
    pub(in crate::prove) out: fs::File,
}

impl Probe {
    fn write_header(&mut self) {
        write!(
            self.out,
            "mid,notation,node_work,node_phi,node_delta,child,chash,work,phi,delta\n"
        )
        .expect("probe header");
    }

    pub(in crate::prove) fn do_probe(
        &mut self,
        tick: Instant,
        mid: usize,
        pos: &game::Game,
        data: &Entry,
        children: &Vec<Child>,
    ) {
        self.tick = tick;

        for (i, ch) in children.iter().enumerate() {
            write!(
                self.out,
                "{},{},{},{},{},{},{},{},{},{}\n",
                mid,
                game::notation::render(pos),
                data.work,
                data.bounds.phi,
                data.bounds.delta,
                i,
                ch.entry.hash,
                ch.entry.work,
                ch.entry.bounds.phi,
                ch.entry.bounds.delta,
            )
            .expect("probe line");
        }
    }

    pub(in crate::prove) fn from_config(cfg: &Config) -> Option<Self> {
        cfg.probe_hash.map(|h| {
            let probelog = fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(&cfg.probe_log)
                .expect("open probe log");
            let mut probe = Probe {
                hash: h,
                tick: Instant::now(),
                out: probelog,
            };
            probe.write_header();
            probe
        })
    }
}

pub(in crate::prove) fn compute_bounds(children: &Vec<Child>) -> Bounds {
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

// returns (child index, Bounds)
pub(in crate::prove) fn select_child(
    children: &Vec<Child>,
    bounds: Bounds,
    data: &mut Entry,
    epsilon: f64,
) -> (usize, Bounds) {
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
    if data.child != std::u8::MAX && data.child != (idx as u8) {
        let child_bounds = thresholds(
            epsilon,
            bounds,
            data.bounds,
            children[data.child as usize].entry.bounds.phi,
            delta_1,
        );
        if children[data.child as usize].entry.bounds.delta < child_bounds.delta {
            return (data.child as usize, child_bounds);
        }
    }
    data.child = idx as u8;

    let child_bounds = thresholds(
        epsilon,
        bounds,
        data.bounds,
        children[idx].entry.bounds.phi,
        delta_2,
    );
    (idx, child_bounds)
}

pub fn thresholds(epsilon: f64, bounds: Bounds, nd: Bounds, phi_1: u32, delta_2: u32) -> Bounds {
    Bounds {
        phi: bounds.delta + phi_1 - nd.delta,
        delta: min(
            bounds.phi,
            max(delta_2 + 1, (delta_2 as f64 * (1.0 + epsilon)) as u32),
        ),
    }
}

pub(in crate::prove) fn populate_pv(data: &mut Entry, children: &Vec<Child>) {
    // If the position is solved, store the PV. For winning moves,
    // find the shortest path to victory; for losing, the
    // most-delaying
    let ch = if data.bounds.phi == 0 {
        children
            .iter()
            .filter(|e| e.entry.bounds.delta == 0)
            .min_by_key(|e| e.entry.work)
            .expect("won node, no winning move")
    } else if data.bounds.delta == 0 {
        children
            .iter()
            .max_by_key(|e| e.entry.work)
            .expect("lost node, no move")
    } else {
        return;
    };
    data.pv = ch.r#move;
}

pub(in crate::prove) fn dump_table<T: table::Table<Entry>>(
    cfg: &Config,
    table: &T,
) -> io::Result<()> {
    if let Some(ref path) = cfg.dump_table {
        let before = Instant::now();
        let mut f = fs::OpenOptions::new()
            .create(true)
            .append(false)
            .truncate(false)
            .write(true)
            .open(path)?;
        table.dump(&mut f)?;

        let elapsed = Instant::now().duration_since(before);
        if cfg.debug > 0 {
            eprintln!(
                "Checkpointed table[{}] in {}.{:03}s",
                path,
                elapsed.as_secs(),
                elapsed.subsec_millis(),
            );
        }
    }
    Ok(())
}

pub(in crate::prove) fn extract_pv<T: table::Table<Entry>>(
    cfg: &Config,
    table: &mut T,
    root: &game::Game,
) -> Vec<game::Move> {
    let mut pv = Vec::new();
    let mut g = root.clone();
    let mut prev = false;
    loop {
        match g.game_state() {
            game::BoardState::InPlay => (),
            _ => {
                if cfg.debug > 0 {
                    eprintln!("PV terminated depth={} game={:?}", pv.len(), g.game_state());
                }
                break;
            }
        };
        if let Some(ent) = table.lookup(g.zobrist()) {
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
            let won = ent.bounds.phi == 0;
            if pv.len() != 0 {
                assert!(
                    won == !prev,
                    "inconsistent game tree d={} prev={} bounds=({}, {})",
                    pv.len(),
                    prev,
                    ent.bounds.phi,
                    ent.bounds.delta
                );
            }
            prev = won;
            if ent.pv.is_none() {
                if cfg.debug > 0 {
                    eprintln!("PV terminated, no move");
                }
                break;
            }
            pv.push(ent.pv);
            g = g.make_move(ent.pv).unwrap_or_else(|_| {
                panic!("PV contained illegal move depth={} m={}", pv.len(), ent.pv)
            });
        } else {
            if cfg.debug > 0 {
                eprintln!("PV terminated depth={} no entry", pv.len());
            }
            break;
        }
    }
    pv
}

pub(in crate::prove) trait ProbeFn = FnMut(&Stats, &game::Game, &Entry, &Vec<Child>);

pub(in crate::prove) struct MID<'a, Table, Probe>
where
    Table: table::Table<Entry>,
    Probe: ProbeFn,
{
    pub(in crate::prove) id: usize,
    pub(in crate::prove) cfg: &'a Config,
    pub(in crate::prove) table: Table,
    pub(in crate::prove) player: game::Player,
    pub(in crate::prove) stack: Vec<game::Move>,
    pub(in crate::prove) probe: Probe,
    pub(in crate::prove) minimax: minimax::Minimax,
    pub(in crate::prove) stats: Stats,
}

impl<'a, Table, Probe> MID<'a, Table, Probe>
where
    Table: table::Table<Entry>,
    Probe: ProbeFn,
{
    fn try_minimax(&mut self, pos: &game::Game) -> Option<bool> {
        self.stats.minimax += 1;
        let (_aipv, aistats) = self.minimax.analyze(pos);
        if let Some(st) = aistats.last() {
            if st.score >= minimax::EVAL_WON {
                self.stats.minimax_solve += 1;
                return Some(true);
            } else if st.score <= minimax::EVAL_LOST {
                self.stats.minimax_solve += 1;
                return Some(false);
            }
        }
        return None;
    }

    pub(in crate::prove) fn mid(
        &mut self,
        bounds: Bounds,
        max_work: u64,
        mut data: Entry,
        pos: &game::Game,
    ) -> (Entry, u64, Vec<Child>) {
        let depth = self.stack.len();
        if self.cfg.debug > 6 {
            eprintln!(
                "{:4$}[{}]mid[{}]: mid={} d={} bounds=({}, {}) max_work={}",
                "",
                self.id,
                self.stack
                    .last()
                    .map(|&m| game::notation::render_move(m))
                    .unwrap_or_else(|| "<root>".to_owned()),
                self.stats.mid,
                depth,
                bounds.phi,
                bounds.delta,
                max_work,
            );
        }
        self.stats.mid += 1;

        if data.bounds.exceeded(bounds) {
            return (data, 0, Vec::new());
        }

        self.stats
            .open_boards
            .record(pos.open_boards() as u64)
            .unwrap();

        let analysis = endgame::Analysis::new(pos, &mut self.stats.endgame);

        let terminal = match pos.game_state() {
            game::BoardState::InPlay => {
                let proof = analysis.status();
                if !proof.is_winnable(self.player) {
                    self.stats.endgame_solve += 1;
                    Some(pos.player() != self.player)
                } else if proof.is_won(self.player) {
                    self.stats.endgame_solve += 1;
                    Some(pos.player() == self.player)
                } else if pos.bound_depth() <= self.cfg.minimax_cutoff {
                    self.try_minimax(pos)
                } else {
                    None
                }
            }
            game::BoardState::Drawn => Some(pos.player() != self.player),
            game::BoardState::Won(p) => Some(p == pos.player()),
        };
        if let Some(v) = terminal {
            if v {
                data.bounds = Bounds::winning();
            } else {
                data.bounds = Bounds::losing();
            }
            data.work = 1;
            self.table.store(&data);
            return (data, 1, Vec::new());
        }

        let mut local_work = 1;

        let mut children = Vec::new();
        for m in pos.all_moves() {
            let g = pos.make_move(m).expect("all_moves returned illegal move");
            let eval = analysis.evaluate_move(m);
            let data = self.ttlookup_or_default(&g, eval);
            let bounds = data.bounds;
            children.push(Child {
                position: g,
                r#move: m,
                entry: data,
            });
            if bounds.delta == 0 {
                break;
            }
        }
        self.stats.branch.record(children.len() as u64).unwrap();

        loop {
            data.bounds = compute_bounds(&children);
            (self.probe)(&self.stats, &pos, &data, &children);

            if local_work >= max_work || data.bounds.exceeded(bounds) {
                break;
            }
            let (best_idx, child_bounds) =
                select_child(&children, bounds, &mut data, self.cfg.epsilon);
            let child = &children[best_idx];
            self.stack.push(children[best_idx].r#move);
            let (child_entry, child_work, _) = self.mid(
                child_bounds,
                max_work - local_work,
                children[best_idx].entry.clone(),
                &child.position,
            );
            children[best_idx].entry = child_entry;
            self.stack.pop();
            local_work += child_work;
            data.work += child_work;
        }

        if data.bounds.solved() {
            self.stats.solved += 1;
        }

        populate_pv(&mut data, &children);
        let did_store = self.table.store(&data);
        if self.cfg.debug > 6 {
            eprintln!(
                "{:depth$}[{id}]exit mid bounds={bounds:?} local_work={local_work} store={did_store}",
                "",
                id=self.id,
                depth = self.stack.len(),
                bounds = data.bounds,
                local_work = local_work,
                did_store = did_store,
            );
        }
        (data, local_work, children)
    }

    pub(in crate::prove) fn ttlookup_or_default(
        &mut self,
        g: &game::Game,
        eval: prove::Status,
    ) -> Entry {
        let bounds = match g.game_state() {
            game::BoardState::Won(p) => {
                if p == g.player() {
                    Bounds::winning()
                } else {
                    Bounds::losing()
                }
            }
            game::BoardState::Drawn => {
                if g.player() == self.player {
                    Bounds::losing()
                } else {
                    Bounds::winning()
                }
            }
            game::BoardState::InPlay => {
                if eval.is_won(g.player()) {
                    self.stats.endgame_move += 1;
                    Bounds::winning()
                } else if eval.is_won(g.player().other()) {
                    self.stats.endgame_move += 1;
                    Bounds::losing()
                } else {
                    Bounds::unity()
                }
            }
        };
        let default = Entry {
            bounds: bounds,
            hash: g.zobrist(),
            work: 0,
            ..Default::default()
        };
        if default.bounds.solved() {
            default
        } else {
            self.table.lookup(g.zobrist()).unwrap_or(default)
        }
    }
}
