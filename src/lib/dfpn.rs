use crate::game;
use crate::prove;
use crate::table;
use std::cmp::{max, min};
use std::collections::hash_map::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};
use typenum;

#[derive(Clone, Debug, Default)]
pub struct Stats {
    pub mid: usize,
    pub terminal: usize,
    pub jobs: usize,
}

impl Stats {
    fn merge(&self, other: &Stats) -> Stats {
        Stats {
            mid: self.mid + other.mid,
            terminal: self.terminal + other.terminal,
            jobs: self.jobs + other.jobs,
        }
    }
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

    fn exceeded(&self, other: Bounds) -> bool {
        self.phi >= other.phi || self.delta >= other.delta
    }

    fn solved(&self) -> bool {
        self.phi == 0 || self.delta == 0
    }
}

#[derive(Clone)]
struct Entry {
    bounds: Bounds,
    hash: u64,
    work: u64,
    pv: game::Move,
    child: u8,
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
            child: std::u8::MAX,
        }
    }
}

#[derive(Clone)]
pub struct Config {
    pub table_size: usize,
    pub timeout: Option<Duration>,
    pub debug: usize,
    pub epsilon: f64,
    pub max_work_per_job: u64,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            table_size: table::DEFAULT_TABLE_SIZE,
            timeout: None,
            debug: 0,
            epsilon: 1.0 / 8.0,
            max_work_per_job: 500,
        }
    }
}

pub struct ProveResult {
    pub value: prove::Evaluation,
    pub bounds: Bounds,
    pub stats: Stats,
    pub ttstats: table::Stats,
    pub duration: Duration,
    pub work: u64,
    pub pv: Vec<game::Move>,
}

pub struct DFPN {
    start: Instant,
    tick: Instant,
    cfg: Config,
    table: table::ConcurrentTranspositionTable<Entry, typenum::U4>,
    stats: Stats,
    root: game::Game,
    vtable: Mutex<HashMap<u64, VEntry>>,
}

#[derive(Clone)]
struct Child {
    position: game::Game,
    r#move: game::Move,
    entry: Entry,
    r#virtual: bool,
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
            table: table::ConcurrentTranspositionTable::with_memory(cfg.table_size),
            stats: Default::default(),
            vtable: Mutex::new(HashMap::new()),
        };
        let mut worker = Worker {
            cfg: &prover.cfg,
            guard: YieldableGuard::new(&prover.vtable),
            table: &prover.table,
            stats: Default::default(),
            stack: Vec::new(),
        };
        let mut root = Entry {
            bounds: Bounds::unity(),
            hash: g.zobrist(),
            work: 0,
            ..Default::default()
        };
        let mut vroot = VPath {
            parent: None,
            children: Vec::new(),
            entry: root.clone(),
        };
        let mut work = 0;
        loop {
            let (result, this_work, did_job) = worker.try_run_job(
                Bounds {
                    phi: INFINITY / 2,
                    delta: INFINITY / 2,
                },
                &g,
                root,
                &mut vroot,
            );
            if !did_job {
                panic!("single-threaded could not find job");
            }
            debug_assert!(worker.guard.is_empty(), "Leaking VEntry's");
            root = result;
            work += this_work;
            if root.bounds.solved() {
                break;
            }
        }
        prover.stats = prover.stats.merge(&worker.stats);
        ProveResult {
            value: if root.bounds.phi == 0 {
                prove::Evaluation::True
            } else if root.bounds.delta == 0 {
                prove::Evaluation::False
            } else {
                prove::Evaluation::Unknown
            },
            work: work,
            bounds: root.bounds,
            stats: prover.stats.clone(),
            ttstats: prover.table.stats(),
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
}

struct YieldableGuard<'a, T> {
    lock: &'a Mutex<T>,
    guard: Option<MutexGuard<'a, T>>,
}

impl<'a, T> YieldableGuard<'a, T> {
    fn new(lk: &'a Mutex<T>) -> Self {
        YieldableGuard {
            lock: lk,
            guard: Some(lk.lock().unwrap()),
        }
    }

    fn drop_lock(&mut self) {
        debug_assert!(self.guard.is_some());

        self.guard = None;
    }

    fn acquire_lock(&mut self) {
        debug_assert!(self.guard.is_none());
        self.guard = Some(self.lock.lock().unwrap());
    }
}

impl<'a, T> Deref for YieldableGuard<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.guard.as_ref().unwrap().deref()
    }
}

impl<'a, T> DerefMut for YieldableGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.guard.as_mut().unwrap().deref_mut()
    }
}

struct Worker<'a> {
    cfg: &'a Config,
    guard: YieldableGuard<'a, HashMap<u64, VEntry>>,
    table: &'a table::ConcurrentTranspositionTable<Entry, typenum::U4>,
    stats: Stats,
    stack: Vec<game::Move>,
}

impl Worker<'_> {
    fn mid(
        &mut self,
        bounds: Bounds,
        max_work: u64,
        mut data: Entry,
        pos: &game::Game,
    ) -> (Entry, u64) {
        if self.cfg.debug > 5 {
            eprintln!(
                "mid[{}]: m={} d={} bounds=({}, {}) max_work={}",
                self.stack
                    .last()
                    .map(|&m| game::notation::render_move(m))
                    .unwrap_or_else(|| "<root>".to_owned()),
                self.stats.mid,
                self.stack.len(),
                bounds.phi,
                bounds.delta,
                max_work,
            );
        }
        self.stats.mid += 1;
        debug_assert!(
            !data.bounds.exceeded(bounds),
            "inconsistent mid call d={}, bounds=({}, {}) me=({}, {}) w={}",
            self.stack.len(),
            bounds.phi,
            bounds.delta,
            data.bounds.phi,
            data.bounds.delta,
            data.work,
        );

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
            self.table.store(&data);
            return (data, 1);
        }

        let mut local_work = 1;

        let mut children = Vec::new();
        for m in pos.all_moves() {
            let g = pos.make_move(m).expect("all_moves returned illegal move");
            let data = ttlookup_or_default(&self.table, &g);
            let bounds = data.bounds;
            children.push(Child {
                position: g,
                r#move: m,
                entry: data,
                r#virtual: false,
            });
            if bounds.delta == 0 {
                break;
            }
        }

        // recurse

        loop {
            data.bounds = compute_bounds(&children);
            if local_work >= max_work || data.bounds.exceeded(bounds) {
                break;
            }
            let (best_idx, child_bounds) =
                select_child(&children, bounds, &mut data, self.cfg.epsilon);
            let child = &children[best_idx];
            self.stack.push(children[best_idx].r#move);
            let (child_entry, child_work) = self.mid(
                child_bounds,
                max_work - local_work,
                children[best_idx].entry.clone(),
                &child.position,
            );
            children[best_idx].entry = child_entry;
            self.stack.pop();
            local_work += child_work;
        }

        data.work += local_work;

        populate_pv(&mut data, &children);
        self.table.store(&data);
        (data, local_work)
    }

    fn try_run_job(
        &mut self,
        bounds: Bounds,
        pos: &game::Game,
        mut data: Entry,
        mut vdata: &mut VPath,
    ) -> (Entry, u64, bool) {
        debug_assert!(
            !vdata.entry.bounds.exceeded(bounds),
            "inconsistent select_next_job call"
        );
        if self.cfg.debug > 4 {
            eprintln!(
                "{:1$}try_run_job: d={} bounds=({}, {}) node=({}, {}) vnode=({}, {}) w={}",
                "",
                vdata.depth(),
                bounds.phi,
                bounds.delta,
                data.bounds.phi,
                data.bounds.delta,
                vdata.entry.bounds.phi,
                vdata.entry.bounds.delta,
                data.work,
            );
        }

        let mut local_work = 0;

        if data.work < self.cfg.max_work_per_job {
            let vnode = self.vadd(&vdata.entry);
            if data.bounds.phi < data.bounds.delta {
                // TODO: does < / <= matter here?
                vnode.entry.bounds = Bounds::winning();
            } else {
                vnode.entry.bounds = Bounds::losing();
            }

            self.update_parents(vdata.parent);
            self.stats.jobs += 1;
            if self.cfg.debug > 3 {
                eprintln!(
                    "{:1$}try_run_job: mid[d={}]({}, {})",
                    "",
                    vdata.depth(),
                    bounds.phi,
                    bounds.delta,
                );
            }
            self.guard.drop_lock();
            let (result, local_work) = self.mid(bounds, self.cfg.max_work_per_job, data, pos);
            self.guard.acquire_lock();

            self.vremove(pos.zobrist());
            return (result, local_work, true);
        }

        // build children
        let mut children = Vec::new();
        vdata.children.clear();
        for m in pos.all_moves() {
            let g = pos.make_move(m).expect("all_moves returned illegal move");
            let entry = ttlookup_or_default(&self.table, &g);
            let child = Child {
                position: g,
                r#move: m,
                entry: entry,
                r#virtual: false,
            };
            let vchild = self
                .guard
                .get(&child.entry.hash)
                .map(|ve| Child {
                    entry: ve.entry.clone(),
                    r#move: m,
                    position: child.position.clone(),
                    r#virtual: true,
                })
                .unwrap_or_else(|| child.clone());

            vdata.children.push(vchild);
            children.push(child);
        }
        debug_assert_eq!(vdata.children.len(), children.len());
        // recurse
        let mut did_job = false;
        loop {
            if did_job {
                for (i, child) in children.iter_mut().enumerate() {
                    if let Some(e) = self.table.lookup(child.position.zobrist()) {
                        child.entry = e;
                    }
                    if let Some(v) = self.guard.get(&child.position.zobrist()) {
                        vdata.children[i].entry = v.entry.clone();
                    }
                }
            }
            data.bounds = compute_bounds(&children);
            vdata.entry.bounds = compute_bounds(&vdata.children);
            populate_pv(&mut data, &children);
            self.table.store(&data);
            if vdata.entry.bounds.exceeded(bounds) || did_job {
                break;
            }
            let (idx, child_bounds) =
                select_child(&vdata.children, bounds, &mut vdata.entry, self.cfg.epsilon);
            let child_data = vdata.children[idx].entry.clone();
            let mut vchild = VPath {
                parent: Some(vdata as *mut VPath),
                children: Vec::new(),
                entry: child_data,
            };

            self.stack.push(children[idx].r#move);
            let (child_entry, child_work, ran) = self.try_run_job(
                child_bounds,
                &children[idx].position,
                children[idx].entry.clone(),
                &mut vchild,
            );
            self.stack.pop();
            did_job = ran;

            local_work += child_work;
            data.work += child_work;
            if !vdata.children[idx].r#virtual {
                vdata.children[idx].entry = child_entry.clone();
            }
            children[idx].entry = child_entry;
        }

        if did_job {
            self.vremove(data.hash);
        }

        (data, local_work, did_job)
    }

    fn vadd(&mut self, entry: &Entry) -> &mut VEntry {
        let vnode = self.guard.entry(entry.hash).or_insert(VEntry {
            entry: entry.clone(),
            saved: Vec::new(),
            count: 0,
        });
        vnode.saved.push(vnode.entry.bounds);
        vnode.count += 1;
        vnode
    }

    fn vremove(&mut self, hash: u64) {
        let entry = self.guard.get_mut(&hash).unwrap();
        entry.count -= 1;
        entry.entry.bounds = entry.saved.pop().unwrap();
        let del = entry.count == 0;
        if del {
            self.guard.remove(&hash);
        }
    }

    fn update_parents(&mut self, mut node: Option<*mut VPath>) {
        while let Some(ptr) = node {
            let v = unsafe { ptr.as_mut().unwrap() };
            self.vadd(&v.entry).entry.bounds = compute_bounds(&v.children);
            node = v.parent;
        }
    }
}

struct VEntry {
    entry: Entry,
    saved: Vec<Bounds>,
    count: isize,
}

struct VPath {
    parent: Option<*mut VPath>,
    entry: Entry,
    children: Vec<Child>,
}

impl VPath {
    fn depth(&self) -> usize {
        let mut d = 0;
        let mut n = self as *const VPath;
        while let Some(p) = unsafe { (*n).parent } {
            d += 1;
            n = p;
        }
        d
    }
}

fn compute_bounds(children: &Vec<Child>) -> Bounds {
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
fn select_child(
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

fn thresholds(epsilon: f64, bounds: Bounds, nd: Bounds, phi_1: u32, delta_2: u32) -> Bounds {
    Bounds {
        phi: bounds.delta + phi_1 - nd.delta,
        delta: min(
            bounds.phi,
            max(delta_2 + 1, (delta_2 as f64 * (1.0 + epsilon)) as u32),
        ),
    }
}

fn ttlookup_or_default<N>(
    table: &table::ConcurrentTranspositionTable<Entry, N>,
    g: &game::Game,
) -> Entry
where
    N: typenum::Unsigned,
{
    let te = table.lookup(g.zobrist());
    te.unwrap_or_else(|| {
        let bounds = match g.game_state() {
            game::BoardState::Won(p) => {
                if p == g.player() {
                    Bounds::winning()
                } else {
                    Bounds::losing()
                }
            }
            game::BoardState::Drawn => Bounds::losing(),
            game::BoardState::InPlay => Bounds::unity(),
        };
        Entry {
            bounds: bounds,
            hash: g.zobrist(),
            work: 0,
            ..Default::default()
        }
    })
}

fn populate_pv(data: &mut Entry, children: &Vec<Child>) {
    // If the position is solved, store the PV. For winning moves,
    // find the shortest path to victory; for losing, the
    // most-delaying
    if data.bounds.phi == 0 {
        data.pv = children
            .iter()
            .filter(|e| e.entry.bounds.delta == 0)
            .min_by_key(|e| e.entry.work)
            .map(|e| e.r#move)
            .expect("won node, no winning move");
    } else if data.bounds.delta == 0 {
        data.pv = children
            .iter()
            .max_by_key(|e| e.entry.work)
            .map(|e| e.r#move)
            .expect("lost node, no move");
    }
}
