extern crate crossbeam;
extern crate parking_lot;

use crate::game;
use crate::minimax;
use crate::prove;
use crate::table;
use parking_lot::{Condvar, Mutex, MutexGuard, RwLock};
use serde::Serialize;
use std::cmp::{max, min};
use std::collections::hash_map::HashMap;
use std::io::Write;
use std::ops::{Deref, DerefMut};
use std::time::{Duration, Instant};
use std::{fmt, fs, io};
use typenum;

#[derive(Clone, Debug, Default, Serialize)]
pub struct Histogram {
    pub counts: Vec<usize>,
}

impl Histogram {
    pub fn merge(&self, other: &Histogram) -> Histogram {
        let mut out = Vec::new();
        out.resize(max(self.counts.len(), other.counts.len()), 0);
        for (i, n) in self.counts.iter().enumerate() {
            out[i] += n;
        }
        for (i, n) in other.counts.iter().enumerate() {
            out[i] += n;
        }
        Histogram { counts: out }
    }

    pub fn inc(&mut self, idx: usize) {
        if self.counts.len() <= idx {
            self.counts.resize(idx + 1, 0);
        }
        self.counts[idx] += 1;
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Stats {
    pub mid: usize,
    pub terminal: usize,
    pub try_calls: usize,
    pub jobs: usize,
    pub try_depth: Histogram,
    pub mid_depth: Histogram,
    pub winning_child: Histogram,
    pub branch: Histogram,
    pub minimax: usize,
    pub minimax_solve: usize,
    pub tt: table::Stats,
}

impl Stats {
    fn merge(&self, other: &Stats) -> Stats {
        Stats {
            mid: self.mid + other.mid,
            terminal: self.terminal + other.terminal,
            jobs: self.jobs + other.jobs,
            try_calls: self.try_calls + other.try_calls,
            minimax: self.minimax + other.minimax,
            minimax_solve: self.minimax_solve + other.minimax_solve,
            tt: self.tt.merge(&other.tt),
            mid_depth: self.mid_depth.merge(&other.mid_depth),
            try_depth: self.try_depth.merge(&other.try_depth),
            winning_child: self.winning_child.merge(&other.winning_child),
            branch: self.branch.merge(&other.branch),
        }
    }
}

const INFINITY: u32 = 1 << 31;

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub struct Bounds {
    pub phi: u32,
    pub delta: u32,
}

impl fmt::Debug for Bounds {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.phi, self.delta)
    }
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
#[repr(C)]
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
    pub write_metrics: Option<String>,
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
            write_metrics: None,
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
        let table = if let Some(ref path) = cfg.load_table {
            table::ConcurrentTranspositionTable::from_file(path).expect("invalid table file")
        } else {
            table::ConcurrentTranspositionTable::with_memory(cfg.table_size)
        };
        let mut prover = DFPN {
            root: g.clone(),
            start: start,
            cfg: cfg.clone(),
            table: table,
            stats: Default::default(),
            vtable: Mutex::new(HashMap::new()),
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
        let probe = if let Some(h) = self.cfg.probe_hash {
            let probelog = fs::OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(&self.cfg.probe_log)
                .expect("open probe log");
            let mut probe = Probe {
                hash: h,
                tick: self.start,
                out: probelog,
            };
            {
                write!(
                    probe.out,
                    "mid,node_work,node_phi,node_delta,child,work,phi,delta\n"
                )
                .expect("probe header");
            }
            Some(probe)
        } else {
            None
        };

        let mmcfg = minimax::Config {
            max_depth: Some(self.cfg.minimax_cutoff as i64 + 1),
            timeout: Some(Duration::from_secs(1)),
            debug: if self.cfg.debug > 6 { 1 } else { 0 },
            table_bytes: None,
            draw_winner: Some(self.root.player().other()),
        };

        if self.cfg.threads == 1 {
            let table = table::TranspositionTable::with_memory(self.cfg.table_size);
            let mut worker = SingleThreadedWorker {
                cfg: &self.cfg,
                player: self.root.player(),
                table: table,
                stats: Default::default(),
                stack: Vec::new(),
                minimax: minimax::Minimax::with_config(&mmcfg),
                probe: probe,
            };
            let root = Entry {
                hash: self.root.zobrist(),
                bounds: Bounds::unity(),
                child: 0xff,
                pv: game::Move::none(),
                work: 0,
            };
            let (out, work) = worker.mid(
                Bounds {
                    phi: INFINITY / 2,
                    delta: INFINITY / 2,
                },
                std::u64::MAX,
                root,
                &self.root,
            );
            self.stats = worker.stats.clone();
            self.stats.tt = worker.table.stats();
            (out, worker.extract_pv(&self.root), work)
        } else {
            let shared = Mutex::new(SharedState {
                vtable: HashMap::new(),
                shutdown: false,
                start: self.start,
                tick: self.start,
                dump_tick: self.start + self.cfg.dump_interval,
            });
            let cv = Condvar::new();

            let mut rwlock: Option<RwLock<Probe>> = None;
            let probe = probe.map(|p| {
                rwlock = Some(RwLock::new(p));
                rwlock.as_ref().unwrap()
            });
            let (work, stats) = crossbeam::scope(|s| {
                let mut guards = Vec::new();
                for i in 0..self.cfg.threads {
                    let player = self.root.player();
                    let root = &self.root;
                    let cfg = &self.cfg;
                    let table = &self.table;
                    let cref = &cv;
                    let sref = &shared;
                    let probe = probe.clone();
                    let mcref = &mmcfg;
                    guards.push(
                        s.builder()
                            .name(format!("worker-{}", i))
                            .spawn(move |_| {
                                let mut worker = Worker {
                                    id: i,
                                    cfg: cfg,
                                    player: player,
                                    guard: YieldableGuard::new(sref),
                                    table: table,
                                    stats: Default::default(),
                                    stack: Vec::new(),
                                    wait: cref,
                                    minimax: minimax::Minimax::with_config(mcref),
                                    probe: probe,
                                };

                                (worker.run(root), worker.stats.clone())
                            })
                            .unwrap(),
                    );
                }
                guards
                    .into_iter()
                    .map(|g| g.join().expect("thread panicked"))
                    .fold(
                        (0, Default::default()),
                        |a: (u64, Stats), b: (u64, Stats)| (a.0 + b.0, a.1.merge(&b.1)),
                    )
            })
            .expect("thread panicked");

            self.stats = self.stats.merge(&stats);
            let pv = Worker {
                // TODO
                id: 0,
                cfg: &self.cfg,
                player: self.root.player(),
                guard: YieldableGuard::new(&shared),
                table: &self.table,
                stats: Default::default(),
                stack: Vec::new(),
                wait: &cv,
                minimax: minimax::Minimax::with_config(&mmcfg),
                probe: probe,
            }
            .extract_pv(&self.root);
            dump_table(&self.cfg, &self.table).expect("final dump_table");
            (
                self.table
                    .lookup(&mut self.stats.tt, self.root.zobrist())
                    .expect("no entry for root"),
                pv,
                work,
            )
        }
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
            guard: Some(lk.lock()),
        }
    }

    fn drop_lock(&mut self) {
        debug_assert!(self.guard.is_some());

        self.guard = None;
    }

    fn acquire_lock(&mut self) {
        debug_assert!(self.guard.is_none());
        self.guard = Some(self.lock.lock());
    }

    fn take(&mut self) -> MutexGuard<'a, T> {
        self.guard.take().unwrap()
    }

    fn wait(&mut self, cond: &Condvar) {
        cond.wait(self.guard.as_mut().unwrap())
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

struct SharedState {
    vtable: HashMap<u64, VEntry>,
    shutdown: bool,
    start: Instant,
    tick: Instant,
    dump_tick: Instant,
}

struct Probe {
    tick: Instant,
    hash: u64,
    out: fs::File,
}

struct Worker<'a> {
    cfg: &'a Config,
    id: usize,
    player: game::Player,
    guard: YieldableGuard<'a, SharedState>,
    table: &'a table::ConcurrentTranspositionTable<Entry, typenum::U4>,
    stats: Stats,
    stack: Vec<game::Move>,
    wait: &'a Condvar,
    minimax: minimax::Minimax,
    probe: Option<&'a RwLock<Probe>>,
}

struct SingleThreadedWorker<'a> {
    cfg: &'a Config,
    player: game::Player,
    table: table::TranspositionTable<Entry, typenum::U4>,
    stats: Stats,
    stack: Vec<game::Move>,
    minimax: minimax::Minimax,
    probe: Option<Probe>,
}

#[derive(Serialize)]
struct Metrics<'a> {
    elapsed_ms: u128,
    stats: &'a Stats,
}

impl Worker<'_> {
    fn try_run_job(
        &mut self,
        bounds: Bounds,
        pos: &game::Game,
        mut data: Entry,
        mut vdata: &mut VPath,
    ) -> (Entry, Bounds, u64, bool) {
        if self.cfg.debug > 4 {
            eprintln!(
                "{:depth$}[{}]try_run_job: m={} d={depth} bounds=({}, {}) node=({}, {}) vnode=({}, {}) w={}",
                "",
                self.id,
                self.stack
                    .last()
                    .map(|&m| game::notation::render_move(m))
                    .unwrap_or_else(|| "<root>".to_owned()),
                bounds.phi,
                bounds.delta,
                data.bounds.phi,
                data.bounds.delta,
                vdata.entry.bounds.phi,
                vdata.entry.bounds.delta,
                data.work,
                depth=vdata.depth(),
            );
        }
        debug_assert!(
            !vdata.entry.bounds.exceeded(bounds),
            "inconsistent select_next_job call"
        );

        self.stats.try_calls += 1;
        self.stats.try_depth.inc(self.stack.len());

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
                    "{:2$}[{}]try_run_job: mid[d={}]({}, {}) work={}",
                    "",
                    self.id,
                    vdata.depth(),
                    bounds.phi,
                    bounds.delta,
                    data.work,
                );
            }
            self.guard.drop_lock();
            let (result, local_work) = self.mid(bounds, self.cfg.max_work_per_job, data, pos);
            self.guard.acquire_lock();

            self.vremove(pos.zobrist(), result.bounds);
            let b = result.bounds;
            return (result, b, local_work, true);
        }

        // build children
        let mut children = Vec::new();
        vdata.children.clear();
        for m in pos.all_moves() {
            let g = pos.make_move(m).expect("all_moves returned illegal move");
            let entry = self.ttlookup_or_default(&g);
            let child = Child {
                position: g,
                r#move: m,
                entry: entry,
                r#virtual: false,
            };
            let vchild = self
                .guard
                .vtable
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

        self.stats.branch.inc(children.len());

        let mut did_job = false;
        loop {
            if did_job {
                for (i, child) in children.iter_mut().enumerate() {
                    if let Some(e) = self
                        .table
                        .lookup(&mut self.stats.tt, child.position.zobrist())
                    {
                        child.entry = e;
                    }
                    if let Some(v) = self.guard.vtable.get(&child.position.zobrist()) {
                        vdata.children[i].entry = v.entry.clone();
                    } else {
                        vdata.children[i].entry = child.entry.clone();
                    }
                }
            }
            data.bounds = compute_bounds(&children);
            vdata.entry.bounds = compute_bounds(&vdata.children);
            populate_pv(&mut data, &children);

            self.try_probe(&data, &children);

            if self.cfg.debug > 5 {
                eprintln!(
                    "{0:1$}[{2}]try_run_job[loop]: node=({3}, {4}) vnode=({5}, {6}) w={7}",
                    "",
                    self.stack.len(),
                    self.id,
                    data.bounds.phi,
                    data.bounds.delta,
                    vdata.entry.bounds.phi,
                    vdata.entry.bounds.delta,
                    data.work,
                );
            }

            self.table.store(&mut self.stats.tt, &data);
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
            let (child_result, child_vbounds, child_work, ran) = self.try_run_job(
                child_bounds,
                &children[idx].position,
                children[idx].entry.clone(),
                &mut vchild,
            );
            self.stack.pop();
            did_job = ran;

            local_work += child_work;
            data.work += child_work;

            children[idx].entry = child_result;
            vdata.children[idx].entry.bounds = child_vbounds;

            if children[idx].entry.bounds.delta == 0 {
                self.stats.winning_child.inc(idx);
            }
        }

        if did_job {
            self.vremove(data.hash, vdata.entry.bounds);
        }

        (data, vdata.entry.bounds, local_work, did_job)
    }

    fn run(&mut self, pos: &game::Game) -> u64 {
        let mut work = 0;
        let mut vroot = VPath {
            parent: None,
            children: Vec::new(),
            entry: Default::default(),
        };
        loop {
            let mut root = self
                .table
                .lookup(&mut self.stats.tt, pos.zobrist())
                .unwrap_or(Entry {
                    bounds: Bounds::unity(),
                    hash: pos.zobrist(),
                    work: 0,
                    ..Default::default()
                });
            vroot.entry = self
                .guard
                .vtable
                .get(&root.hash)
                .map(|v| v.entry.clone())
                .unwrap_or_else(|| root.clone());

            let (result, vbounds, this_work, did_job) = if vroot.entry.bounds.solved() {
                (root, vroot.entry.bounds, 0, false)
            } else {
                self.try_run_job(
                    Bounds {
                        phi: INFINITY / 2,
                        delta: INFINITY / 2,
                    },
                    &pos,
                    root,
                    &mut vroot,
                )
            };
            root = result;
            vroot.entry.bounds = vbounds;
            work += this_work;

            if self.cfg.threads == 1 {
                debug_assert!(self.guard.vtable.is_empty(), "leaking ventries");
                debug_assert!(root.bounds == vroot.entry.bounds, "vroot differs from root");
                debug_assert!(
                    did_job || root.bounds.solved(),
                    "single-threaded no progress"
                );
            }

            if self.cfg.debug > 2 && did_job {
                eprintln!(
                    "[{}] top root=({},{}) vroot=({},{}) work={}",
                    self.id,
                    root.bounds.phi,
                    root.bounds.delta,
                    vroot.entry.bounds.phi,
                    vroot.entry.bounds.delta,
                    work,
                );
            }

            let now = Instant::now();
            if self.cfg.debug > 0 && now > self.guard.tick {
                let elapsed = now.duration_since(self.guard.start);
                eprintln!(
                    "[{}]t={}.{:03}s root=({},{}) jobs={} work={}",
                    self.id,
                    elapsed.as_secs(),
                    elapsed.subsec_millis(),
                    root.bounds.phi,
                    root.bounds.delta,
                    self.stats.jobs,
                    work,
                );
                self.guard.tick = now + TICK_TIME;
                if let Some(ref p) = self.cfg.write_metrics {
                    let mut f = fs::OpenOptions::new()
                        .read(false)
                        .append(true)
                        .write(true)
                        .truncate(false)
                        .create(true)
                        .open(p)
                        .expect("write_metrics");
                    let e = Metrics {
                        elapsed_ms: elapsed.as_millis(),
                        stats: &self.stats,
                    };
                    serde_json::to_writer(&mut f, &e).expect("write json");
                    writeln!(f).expect("write newline");
                }
            }

            if let Some(_) = self.cfg.dump_table {
                if now > self.guard.dump_tick {
                    dump_table(&self.cfg, self.table).expect("dump_table failed");
                    self.guard.dump_tick = now + self.cfg.dump_interval;
                }
            }

            if root.bounds.solved() || self.guard.shutdown {
                self.guard.shutdown = true;
                self.wait.notify_all();
                break;
            }

            if did_job {
                self.wait.notify_all();
            } else {
                self.guard.wait(&self.wait);
            }
        }

        work
    }

    fn vadd(&mut self, entry: &Entry) -> &mut VEntry {
        let vnode = self.guard.vtable.entry(entry.hash).or_insert(VEntry {
            entry: entry.clone(),
            count: 0,
        });
        vnode.count += 1;
        vnode
    }

    fn vremove(&mut self, hash: u64, bounds: Bounds) {
        let entry = self.guard.vtable.get_mut(&hash).unwrap();
        entry.count -= 1;
        entry.entry.bounds = bounds;
        let del = entry.count == 0;
        if del {
            self.guard.vtable.remove(&hash);
        }
    }

    fn update_parents(&mut self, mut node: Option<*mut VPath>) {
        while let Some(ptr) = node {
            let v = unsafe { ptr.as_mut().unwrap() };
            self.vadd(&v.entry).entry.bounds = compute_bounds(&v.children);
            node = v.parent;
        }
    }

    fn ttlookup_or_default(&mut self, g: &game::Game) -> Entry {
        let te = self.table.lookup(&mut self.stats.tt, g.zobrist());
        te.unwrap_or_else(|| {
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
}

struct VEntry {
    entry: Entry,
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

fn dump_table<N: typenum::Unsigned>(
    cfg: &Config,
    table: &table::ConcurrentTranspositionTable<Entry, N>,
) -> io::Result<()> {
    if let Some(ref path) = cfg.dump_table {
        let before = Instant::now();
        let mut f = fs::File::create(path)?;
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

trait DFPNWorker {
    fn id(&self) -> usize;
    fn cfg(&self) -> &Config;
    fn stack(&mut self) -> &mut Vec<game::Move>;
    fn stats(&mut self) -> &mut Stats;
    fn player(&self) -> game::Player;
    fn ttlookup(&mut self, hash: u64) -> Option<Entry>;
    fn ttstore(&mut self, entry: &Entry) -> bool;
    fn minimax(&mut self) -> &mut minimax::Minimax;
    fn try_probe(&mut self, data: &Entry, children: &Vec<Child>);

    fn try_minimax(&mut self, pos: &game::Game) -> Option<bool> {
        self.stats().minimax += 1;
        let (_aipv, aistats) = self.minimax().analyze(pos);
        if let Some(st) = aistats.last() {
            if st.score >= minimax::EVAL_WON {
                self.stats().minimax_solve += 1;
                return Some(true);
            } else if st.score <= minimax::EVAL_LOST {
                self.stats().minimax_solve += 1;
                return Some(false);
            }
        }
        return None;
    }

    fn mid(
        &mut self,
        bounds: Bounds,
        max_work: u64,
        mut data: Entry,
        pos: &game::Game,
    ) -> (Entry, u64) {
        let depth = self.stack().len();
        if self.cfg().debug > 6 {
            eprintln!(
                "{:4$}[{}]mid[{}]: mid={} d={} bounds=({}, {}) max_work={}",
                "",
                self.id(),
                self.stack()
                    .last()
                    .map(|&m| game::notation::render_move(m))
                    .unwrap_or_else(|| "<root>".to_owned()),
                self.stats().mid,
                depth,
                bounds.phi,
                bounds.delta,
                max_work,
            );
        }
        self.stats().mid += 1;
        self.stats().mid_depth.inc(depth);

        debug_assert!(
            !data.bounds.exceeded(bounds),
            "inconsistent mid call d={}, bounds=({}, {}) me=({}, {}) w={}",
            self.stack().len(),
            bounds.phi,
            bounds.delta,
            data.bounds.phi,
            data.bounds.delta,
            data.work,
        );

        let terminal = match pos.game_state() {
            game::BoardState::InPlay => {
                if pos.bound_depth() <= self.cfg().minimax_cutoff {
                    self.try_minimax(pos)
                } else {
                    None
                }
            }
            game::BoardState::Drawn => Some(pos.player() != self.player()),
            game::BoardState::Won(p) => Some(p == pos.player()),
        };
        if let Some(v) = terminal {
            if v {
                data.bounds = Bounds::winning();
            } else {
                data.bounds = Bounds::losing();
            }
            self.stats().terminal += 1;
            data.work = 1;
            self.ttstore(&data);
            return (data, 1);
        }

        let mut local_work = 1;

        let mut children = Vec::new();
        for m in pos.all_moves() {
            let g = pos.make_move(m).expect("all_moves returned illegal move");
            let data = self.ttlookup_or_default(&g);
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

        self.stats().branch.inc(children.len());

        loop {
            data.bounds = compute_bounds(&children);
            self.try_probe(&data, &children);

            if local_work >= max_work || data.bounds.exceeded(bounds) {
                break;
            }
            let (best_idx, child_bounds) =
                select_child(&children, bounds, &mut data, self.cfg().epsilon);
            let child = &children[best_idx];
            self.stack().push(children[best_idx].r#move);
            let (child_entry, child_work) = self.mid(
                child_bounds,
                max_work - local_work,
                children[best_idx].entry.clone(),
                &child.position,
            );
            children[best_idx].entry = child_entry;
            self.stack().pop();
            local_work += child_work;

            if children[best_idx].entry.bounds.delta == 0 {
                self.stats().winning_child.inc(best_idx);
            }
        }

        data.work += local_work;

        populate_pv(&mut data, &children);
        let did_store = self.ttstore(&data);
        if self.cfg().debug > 6 {
            eprintln!(
                "{:depth$}[{id}]exit mid bounds={bounds:?} local_work={local_work} store={did_store}",
                "",
                id=self.id(),
                depth = self.stack().len(),
                bounds = data.bounds,
                local_work = local_work,
                did_store = did_store,
            );
        }
        (data, local_work)
    }

    fn ttlookup_or_default(&mut self, g: &game::Game) -> Entry {
        let te = self.ttlookup(g.zobrist());
        te.unwrap_or_else(|| {
            let bounds = match g.game_state() {
                game::BoardState::Won(p) => {
                    if p == g.player() {
                        Bounds::winning()
                    } else {
                        Bounds::losing()
                    }
                }
                game::BoardState::Drawn => {
                    if g.player() == self.player() {
                        Bounds::losing()
                    } else {
                        Bounds::winning()
                    }
                }
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

    fn extract_pv(&mut self, root: &game::Game) -> Vec<game::Move> {
        let mut pv = Vec::new();
        let mut g = root.clone();
        let mut prev = false;
        loop {
            match g.game_state() {
                game::BoardState::InPlay => (),
                _ => {
                    if self.cfg().debug > 0 {
                        eprintln!("PV terminated depth={} game={:?}", pv.len(), g.game_state());
                    }
                    break;
                }
            };
            if let Some(ent) = self.ttlookup(g.zobrist()) {
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
                    if self.cfg().debug > 0 {
                        eprintln!("PV terminated, no move");
                    }
                    break;
                }
                pv.push(ent.pv);
                g = g.make_move(ent.pv).unwrap_or_else(|_| {
                    panic!("PV contained illegal move depth={} m={}", pv.len(), ent.pv)
                });
            } else {
                if self.cfg().debug > 0 {
                    eprintln!("PV terminated depth={} no entry", pv.len());
                }
                break;
            }
        }
        pv
    }
}

impl<'a> DFPNWorker for Worker<'a> {
    fn id(&self) -> usize {
        self.id
    }
    fn cfg(&self) -> &Config {
        &self.cfg
    }
    fn stack(&mut self) -> &mut Vec<game::Move> {
        &mut self.stack
    }
    fn stats(&mut self) -> &mut Stats {
        &mut self.stats
    }
    fn player(&self) -> game::Player {
        self.player
    }
    fn ttlookup(&mut self, hash: u64) -> Option<Entry> {
        self.table.lookup(&mut self.stats.tt, hash)
    }
    fn ttstore(&mut self, entry: &Entry) -> bool {
        self.table.store(&mut self.stats.tt, entry)
    }
    fn minimax(&mut self) -> &mut minimax::Minimax {
        &mut self.minimax
    }

    fn try_probe(&mut self, data: &Entry, children: &Vec<Child>) {
        if let Some(ref mut p) = self.probe {
            let r = p.read();
            if data.hash != r.hash {
                return;
            }
            let now = Instant::now();
            if now < r.tick && !data.bounds.solved() {
                return;
            }
            let mut w = p.write();
            w.tick = now + Duration::from_millis(10);

            for (i, ch) in children.iter().enumerate() {
                write!(
                    w.out,
                    "{},{},{},{},{},{},{},{}\n",
                    self.stats.mid,
                    data.work,
                    data.bounds.phi,
                    data.bounds.delta,
                    i,
                    ch.entry.work,
                    ch.entry.bounds.phi,
                    ch.entry.bounds.delta,
                )
                .expect("probe line");
            }
        }
    }
}

impl<'a> DFPNWorker for SingleThreadedWorker<'a> {
    fn id(&self) -> usize {
        0
    }
    fn cfg(&self) -> &Config {
        &self.cfg
    }
    fn stack(&mut self) -> &mut Vec<game::Move> {
        &mut self.stack
    }
    fn stats(&mut self) -> &mut Stats {
        &mut self.stats
    }
    fn player(&self) -> game::Player {
        self.player
    }
    fn ttlookup(&mut self, hash: u64) -> Option<Entry> {
        self.table.lookup(hash)
    }
    fn ttstore(&mut self, entry: &Entry) -> bool {
        self.table.store(entry)
    }
    fn minimax(&mut self) -> &mut minimax::Minimax {
        &mut self.minimax
    }

    // TODO: unify with Worker::try_probe
    fn try_probe(&mut self, data: &Entry, children: &Vec<Child>) {
        if let Some(ref mut p) = self.probe {
            if data.hash != p.hash {
                return;
            }
            let now = Instant::now();
            if now < p.tick && !data.bounds.solved() {
                return;
            }
            p.tick = now + Duration::from_millis(10);

            for (i, ch) in children.iter().enumerate() {
                write!(
                    p.out,
                    "{},{},{},{},{},{},{},{}\n",
                    self.stats.mid,
                    data.work,
                    data.bounds.phi,
                    data.bounds.delta,
                    i,
                    ch.entry.work,
                    ch.entry.bounds.phi,
                    ch.entry.bounds.delta,
                )
                .expect("probe line");
            }
        }
    }
}
