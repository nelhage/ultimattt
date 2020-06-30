use crate::game;
use crate::minimax;
use crate::progress::Ticker;
use crate::prove::dfpn;
use crate::prove::node_pool;
use crate::prove::node_pool::{NodeID, Pool};
use crate::prove::{Bounds, Evaluation, INFINITY};
use crate::table;
use crate::util;

use crossbeam;
use crossbeam::channel;
use hdrhistogram::Histogram;
use parking_lot::RwLock;
use probe::probe;
use serde::Serialize;

use std::cmp::min;
use std::mem;
use std::mem::MaybeUninit;
use std::sync::atomic::AtomicU32;
use std::time::{Duration, Instant};

#[derive(Clone)]
pub struct Config {
    pub debug: usize,
    pub max_nodes: Option<usize>,
    pub timeout: Option<Duration>,
    pub split_threshold: u64,
    pub dfpn: dfpn::Config,
    pub queue_depth: usize,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            debug: 0,
            max_nodes: None,
            timeout: None,
            dfpn: Default::default(),
            split_threshold: 1_000,
            queue_depth: 0,
        }
    }
}

#[derive(Clone, Serialize)]
pub struct Stats {
    pub proved: usize,
    pub disproved: usize,
    pub expanded: usize,
    pub jobs: usize,
    pub recv: usize,
    pub mid: dfpn::Stats,
    pub allocated: usize,
    pub freed: usize,
    pub epsilon_resume: usize,
    #[serde(flatten)]
    pub worker: WorkerStats,
}

impl Default for Stats {
    fn default() -> Self {
        Stats {
            proved: 0,
            disproved: 0,
            expanded: 0,
            jobs: 0,
            recv: 0,
            allocated: 0,
            freed: 0,
            epsilon_resume: 0,
            mid: Default::default(),
            worker: Default::default(),
        }
    }
}

const FLAG_EXPANDED: u8 = 1 << 0;
const FLAG_AND: u8 = 1 << 1;
const FLAG_FREE: u8 = 1 << 2;

struct Node {
    bounds: Bounds,      // 8
    vbounds: Bounds,     // 8
    work: u64,           // 8
    parent: NodeID,      // 4
    first_child: NodeID, // 4
    sibling: NodeID,     // 4
    r#move: game::Move,  // 1
    pv: game::Move,      // 1
    flags: u8,           // 1
    vcount: u8,          // 1
}

pub fn sizeof_node() -> usize {
    mem::size_of::<Node>()
}

impl Node {
    fn flag(&self, flag: u8) -> bool {
        (self.flags & flag) != 0
    }

    fn set_flag(&mut self, flag: u8) {
        self.flags |= flag;
    }

    fn proof(&self) -> u32 {
        if self.flag(FLAG_AND) {
            self.bounds.delta
        } else {
            self.bounds.phi
        }
    }

    fn disproof(&self) -> u32 {
        if self.flag(FLAG_AND) {
            self.bounds.phi
        } else {
            self.bounds.delta
        }
    }
}

impl node_pool::Node for Node {
    unsafe fn init(ptr: *mut MaybeUninit<Node>, free: NodeID) {
        ptr.write(MaybeUninit::new(Node {
            parent: free,
            r#move: game::Move::none(),
            pv: game::Move::none(),
            bounds: Bounds::unity(),
            vbounds: Bounds::unity(),
            flags: FLAG_FREE,
            first_child: NodeID::none(),
            sibling: NodeID::none(),
            work: 0,
            vcount: 0,
        }));
    }

    fn alloc(&mut self) {
        debug_assert!(self.flag(FLAG_FREE));
        debug_assert!(!self.first_child.exists());
        debug_assert!(self.vcount == 0);
        self.parent = NodeID::none();
        self.bounds = Bounds { phi: 0, delta: 0 };
        self.vbounds = Bounds { phi: 0, delta: 0 };
        self.flags = 0;
    }

    fn free(&mut self) {
        debug_assert!(!self.flag(FLAG_FREE));
        debug_assert!(self.vcount == 0);
        self.flags = FLAG_FREE;
        self.first_child = NodeID::none();
        self.sibling = NodeID::none();
    }

    fn set_free_ptr(&mut self, free: NodeID) {
        debug_assert!(self.flag(FLAG_FREE));
        self.parent = free;
    }

    fn get_free_ptr(&self) -> NodeID {
        debug_assert!(self.flag(FLAG_FREE));
        self.parent
    }
}

pub struct Prover {
    cfg: Config,
    stats: Stats,
    nodes: Pool<Node>,

    start: Instant,
    tick: Ticker,
    limit: Option<Instant>,

    position: game::Game,
    root: NodeID,
}

struct Job {
    nid: NodeID,
    pos: game::Game,
    bounds: Bounds,
    node: Bounds,
}

struct JobResult {
    nid: NodeID,
    work: u64,
    bounds: Bounds,
    children: Vec<dfpn::Child>,
}

pub struct ProofResult {
    pub result: Evaluation,
    pub duration: Duration,
    pub proof: u32,
    pub disproof: u32,

    pub stats: Stats,
}

static TICK_INTERVAL: Duration = Duration::from_millis(1000);

#[derive(Clone, Serialize)]
pub struct WorkerStats {
    #[serde(serialize_with = "util::serialize_histogram")]
    pub work_per_job: Histogram<u64>,
    #[serde(serialize_with = "util::serialize_histogram")]
    pub us_per_job: Histogram<u64>,
}

impl Default for WorkerStats {
    fn default() -> Self {
        WorkerStats {
            work_per_job: Histogram::new(3).unwrap(),
            us_per_job: Histogram::new(3).unwrap(),
        }
    }
}

impl WorkerStats {
    fn merge(&self, rhs: &Self) -> Self {
        WorkerStats {
            work_per_job: util::merge_histogram(&self.work_per_job, &rhs.work_per_job),
            us_per_job: util::merge_histogram(&self.us_per_job, &rhs.us_per_job),
        }
    }
}

struct Worker<'a, Table, Probe>
where
    Table: table::Table<dfpn::Entry>,
    Probe: dfpn::ProbeFn,
{
    cfg: &'a Config,
    jobs: crossbeam::Receiver<Job>,
    results: crossbeam::Sender<JobResult>,
    mid: dfpn::MID<'a, Table, Probe>,
    stats: WorkerStats,
}

impl<'a, Table, Probe> Worker<'a, Table, Probe>
where
    Table: table::Table<dfpn::Entry>,
    Probe: dfpn::ProbeFn,
{
    fn run(&mut self) {
        for job in self.jobs.iter() {
            probe!(pndfpn, enter_run_job);
            let start = Instant::now();
            let (entry, work, children) = self.mid.mid(
                job.bounds,
                self.cfg.split_threshold,
                dfpn::Entry {
                    hash: job.pos.zobrist(),
                    bounds: job.node,
                    child: 0xff,
                    pv: game::Move::none(),
                    work: 0,
                    sync: AtomicU32::new(0),
                },
                &job.pos,
            );
            let t = Instant::now().duration_since(start);
            self.stats.us_per_job.record(t.as_micros() as u64).unwrap();
            self.stats.work_per_job.record(work).unwrap();
            probe!(pndfpn, exit_run_job);

            if let Err(_) = self.results.send(JobResult {
                nid: job.nid,
                work: work,
                bounds: entry.bounds,
                children: children,
            }) {
                break;
            }
        }
    }
}

struct Cursor {
    nid: NodeID,
    pos: game::Game,
    bounds: Bounds,
}

impl Prover {
    pub fn prove(cfg: &Config, pos: &game::Game) -> ProofResult {
        if cfg.debug > 1 {
            eprintln!("Entering prove() sizeof(Node)={}", mem::size_of::<Node>());
        }
        let start = Instant::now();
        let mut prover = Prover {
            cfg: cfg.clone(),
            stats: Default::default(),
            nodes: Pool::new(),
            start: start,
            tick: Ticker::new(TICK_INTERVAL),
            limit: cfg.timeout.map(|t| Instant::now() + t),
            position: pos.clone(),
            root: NodeID::none(),
        };

        {
            let mut root = prover.nodes.alloc();

            root.parent = NodeID::none();
            prover.root = root.id;
            prover.evaluate(&mut root, pos);
        }

        let table = if let Some(ref path) = cfg.dfpn.load_table {
            table::ConcurrentTranspositionTable::<dfpn::Entry, 4>::from_file(path)
                .expect("unable to load table from file")
        } else {
            table::ConcurrentTranspositionTable::<dfpn::Entry, 4>::with_memory(cfg.dfpn.table_size)
        };

        let mut rwlock: Option<RwLock<dfpn::Probe>> = None;
        let probe = dfpn::Probe::from_config(&cfg.dfpn).map(|p| {
            rwlock = Some(RwLock::new(p));
            rwlock.as_ref().unwrap()
        });

        crossbeam::scope(|s| {
            let (job_send, job_recv) = channel::bounded(prover.queue_depth());
            let (res_send, res_recv) = channel::bounded(prover.queue_depth());
            let mmcfg = minimax::Config {
                max_depth: Some(cfg.dfpn.minimax_cutoff as i64 + 1),
                timeout: Some(Duration::from_secs(1)),
                debug: if cfg.debug > 6 { 1 } else { 0 },
                table_bytes: None,
                draw_winner: Some(pos.player().other()),
            };

            let handles = (0..cfg.dfpn.threads)
                .map(|i| {
                    let mut worker = Worker {
                        cfg: cfg,
                        jobs: job_recv.clone(),
                        results: res_send.clone(),
                        mid: dfpn::MID {
                            id: i,
                            cfg: &cfg.dfpn,
                            table: table.handle(),
                            player: pos.player(),
                            stack: Vec::new(),
                            probe:
                                |stats: &dfpn::Stats,
                                 pos: &game::Game,
                                 data: &dfpn::Entry,
                                 children: &Vec<dfpn::Child>| {
                                    if let Some(ref p) = probe {
                                        {
                                            let r = p.read();
                                            if data.hash != r.hash {
                                                return;
                                            }
                                        }

                                        let mut w = p.write();
                                        let tick = w.tick;
                                        w.do_probe(tick, stats.mid, pos, data, children);
                                    }
                                },
                            minimax: minimax::Minimax::with_config(&mmcfg),
                            stats: Default::default(),
                        },
                        stats: Default::default(),
                    };
                    s.spawn(move |_| {
                        worker.run();
                        (worker.mid.stats, worker.stats)
                    })
                })
                .collect::<Vec<_>>();

            mem::drop(job_recv);

            prover.search(
                job_send,
                res_recv,
                &table.handle(),
                prover.root,
                prover.cfg.max_nodes,
            );

            let (mid_stats, worker_stats) = handles.into_iter().map(|h| h.join().unwrap()).fold(
                Default::default(),
                |l: (dfpn::Stats, WorkerStats), r: (dfpn::Stats, WorkerStats)| {
                    (l.0.merge(&r.0), l.1.merge(&r.1))
                },
            );
            prover.stats.mid = mid_stats;
            prover.stats.worker = worker_stats;
        })
        .unwrap();

        prover.stats.mid.tt = table.stats();
        prover.stats.allocated = prover.nodes.stats.allocated.get();
        prover.stats.freed = prover.nodes.stats.freed.get();

        let ref root = prover.nodes.get(prover.root);
        ProofResult {
            result: match (root.proof(), root.disproof()) {
                (0, _) => Evaluation::True,
                (_, 0) => Evaluation::False,
                _ => Evaluation::Unknown,
            },
            duration: Instant::now() - prover.start,
            proof: root.proof(),
            disproof: root.disproof(),
            stats: prover.stats.clone(),
        }
    }

    fn queue_depth(&self) -> usize {
        if self.cfg.queue_depth > 0 {
            self.cfg.queue_depth
        } else {
            self.cfg.dfpn.threads + 1
        }
    }
    fn evaluate(&self, node: &mut node_pool::AllocedNode<Node>, pos: &game::Game) {
        let player = self.player();
        let res = pos.game_state();
        let value = match res {
            game::BoardState::InPlay => Evaluation::Unknown,
            game::BoardState::Drawn => {
                if pos.player() == player {
                    Evaluation::False
                } else {
                    Evaluation::True
                }
            }
            game::BoardState::Won(p) => {
                if p == player {
                    Evaluation::True
                } else {
                    Evaluation::False
                }
            }
        };
        let bounds = match value {
            Evaluation::True | Evaluation::False => {
                if node.flag(FLAG_AND) == (value == Evaluation::True) {
                    Bounds::losing()
                } else {
                    Bounds::winning()
                }
            }
            Evaluation::Unknown => {
                Bounds::unity()
                // delta = pos.all_moves().count() as u32;
                // delta = pos.bound_depth() as u32;
            }
        };
        node.bounds = bounds;
        node.vbounds = bounds;
    }

    /*
    fn set_proof_numbers(&self, nid: NodeID, node: &mut Node) {
        let (bounds, vbounds, work) = self.calc_proof_numbers(nid, node);
        node.bounds = bounds;
        node.vbounds = vbounds;
        node.work = work;
    }
    */

    fn set_proof_numbers<'a>(&'a mut self, nid: NodeID) -> &'a mut Node {
        let node = self.nodes.get(nid);
        debug_assert!(node.flag(FLAG_EXPANDED));
        let mut work = 0;
        let mut vcount = 0;
        let mut bounds = Bounds {
            delta: 0,
            phi: std::u32::MAX,
        };

        let mut vbounds = Bounds {
            delta: 0,
            phi: std::u32::MAX,
        };

        let mut c = node.first_child;
        while c.exists() {
            let node = self.nodes.get(c);
            bounds.delta = bounds.delta.saturating_add(node.bounds.phi);
            bounds.phi = min(bounds.phi, node.bounds.delta);
            vbounds.delta = vbounds.delta.saturating_add(node.vbounds.phi);
            vbounds.phi = min(vbounds.phi, node.vbounds.delta);
            if node.vcount > 0 {
                vcount += 1;
            }
            c = node.sibling;
            work += node.work;
        }
        let node_mut = self.nodes.get_mut(nid);
        node_mut.bounds = bounds;
        node_mut.vbounds = vbounds;
        node_mut.work = work;
        node_mut.vcount = vcount;
        node_mut
    }

    fn search<T: table::Table<dfpn::Entry>>(
        &mut self,
        jobs: channel::Sender<Job>,
        results: channel::Receiver<JobResult>,
        table: &T,
        root: NodeID,
        limit: Option<usize>,
    ) {
        let mut inflight = 0;
        let mut dump_tick = Ticker::new(self.cfg.dfpn.dump_interval);
        while {
            let ref nd = self.nodes.get(root);
            !nd.bounds.solved()
        } {
            let now = Instant::now();
            if self.tick.tick() && self.cfg.debug > 0 {
                let elapsed = now.duration_since(self.start);
                eprintln!(
                    "t={}.{:03}s nodes={}/{} proved={} disproved={} root=({}, {}) n/s={:.0} rss={}",
                    elapsed.as_secs(),
                    elapsed.subsec_millis(),
                    self.nodes.stats.live(),
                    self.nodes.stats.allocated.get(),
                    self.stats.proved,
                    self.stats.disproved,
                    self.nodes.get(self.root).proof(),
                    self.nodes.get(self.root).disproof(),
                    (self.nodes.stats.allocated.get() as f64) / (elapsed.as_secs() as f64),
                    util::read_rss(),
                );
            }
            if let Some(_) = self.cfg.dfpn.dump_table {
                if dump_tick.tick() {
                    let start = Instant::now();
                    dfpn::dump_table(&self.cfg.dfpn, table).unwrap();
                    let elapsed = Instant::now().duration_since(start);
                    eprintln!(
                        "Dumped table in {}.{:03}s",
                        elapsed.as_secs(),
                        elapsed.subsec_millis()
                    );
                }
            }
            if let Some(limit) = self.limit {
                if now > limit {
                    break;
                }
            }
            if limit.map(|n| self.nodes.stats.live() > n).unwrap_or(false) {
                break;
            }

            while inflight < self.queue_depth() {
                probe!(pndfpn, enter_select_job);
                let candidate = self.select_job();
                probe!(pndfpn, exit_select_job);

                if let Some(job) = candidate {
                    self.stats.jobs += 1;
                    jobs.send(job).unwrap();
                    inflight += 1;
                } else {
                    break;
                }
            }

            self.stats.recv += 1;
            self.handle_result(results.recv().unwrap());
            inflight -= 1;

            self.stats.recv += 1;
            for result in results.try_iter() {
                self.handle_result(result);
                inflight -= 1;
                self.stats.recv += 1;
            }
        }
    }

    fn select_job<'a>(&'a mut self) -> Option<Job> {
        let mpn = self.select_most_proving(Cursor {
            nid: self.root,
            pos: self.position.clone(),
            bounds: Bounds::root(),
        })?;
        /*
        if self.nodes.get(mpn.nid).work >= self.cfg.split_threshold {
            self.expand(&mpn);
            mpn = self.select_most_proving(mpn)?;
            did_split = true;
        }
        */

        let node = self.nodes.get_mut(mpn.nid);
        if node.vbounds.exceeded(mpn.bounds) {
            return None;
        }

        if self.cfg.debug > 2 {
            println!(
                "Select mpn nid={:?} bounds={:?} node={:?} vnode={:?} work={}",
                mpn.nid, mpn.bounds, node.bounds, node.vbounds, node.work,
            )
        }

        let job = Job {
            nid: mpn.nid,
            node: node.bounds,
            bounds: mpn.bounds,
            pos: mpn.pos,
        };
        node.vbounds = if node.bounds.phi < node.bounds.delta {
            Bounds::winning()
        } else {
            Bounds::losing()
        };
        node.vcount = 1;
        let parent = node.parent;
        if parent.exists() {
            self.update_ancestors(parent);
        }
        Some(job)
    }

    fn handle_result(&mut self, res: JobResult) {
        if self.cfg.debug > 2 {
            println!(
                "Got result nid={:?} d={} bounds={:?} work={}",
                res.nid,
                self.depth(res.nid),
                res.bounds,
                res.work,
            );
        }
        let node = self.nodes.get_mut(res.nid);
        node.work += res.work;
        debug_assert!(!node.flag(FLAG_EXPANDED));
        if res.work >= self.cfg.split_threshold && !res.bounds.solved() {
            self.expand(res.nid, res.children);
            self.update_ancestors(res.nid);
        } else {
            node.vcount = 0;
            node.bounds = res.bounds;
            node.vbounds = res.bounds;
            let parent = node.parent;
            if parent.exists() {
                self.update_ancestors(parent);
            }
        }
    }

    fn select_most_proving(&mut self, mut root: Cursor) -> Option<Cursor> {
        while self.nodes.get(root.nid).flag(FLAG_EXPANDED) {
            let ref node = self.nodes.get(root.nid);
            if node.vbounds.solved() {
                return None;
            }
            debug_assert!(
                node.bounds.phi != 0,
                "select mpn phi=0, node={:?} vnode={:?} depth={}",
                node.bounds,
                node.vbounds,
                self.depth(root.nid),
            );
            if self.cfg.dfpn.threads == 1 {
                debug_assert!(
                    node.bounds == node.vbounds,
                    "single-threaded inconsistent bounds={:?} vbounds={:?} d={}",
                    node.bounds,
                    node.vbounds,
                    self.depth(root.nid)
                );
            }
            let mut child = NodeID::none();
            let (mut delta_1, mut delta_2) = (INFINITY, INFINITY);
            let mut c = node.first_child;
            let mut pv: Option<NodeID> = None;
            while c.exists() {
                let ch = self.nodes.get(c);
                if ch.r#move == node.pv {
                    pv = Some(c);
                }
                if ch.vbounds.delta < delta_1 {
                    delta_2 = delta_1;
                    delta_1 = ch.vbounds.delta;
                    child = c;
                } else if ch.vbounds.delta < delta_2 {
                    delta_2 = ch.vbounds.delta;
                }
                c = ch.sibling;
            }
            debug_assert!(child.exists(), "found a child");

            if let Some(pvn) = pv {
                if pvn != child {
                    let nd = self.nodes.get(pvn);
                    let pv_bounds = dfpn::thresholds(
                        self.cfg.dfpn.epsilon,
                        root.bounds,
                        node.vbounds,
                        nd.vbounds.phi,
                        delta_1,
                    );
                    if nd.vbounds.delta < pv_bounds.delta {
                        self.stats.epsilon_resume += 1;
                        root = Cursor {
                            bounds: pv_bounds,
                            pos: root.pos.make_move(nd.r#move).unwrap(),
                            nid: pvn,
                        };
                        continue;
                    }
                }
            }
            let cn = self.nodes.get(child);
            let m = cn.r#move;
            let nid = root.nid;
            root = Cursor {
                bounds: dfpn::thresholds(
                    self.cfg.dfpn.epsilon,
                    root.bounds,
                    node.vbounds,
                    cn.vbounds.phi,
                    delta_2,
                ),
                pos: root.pos.make_move(cn.r#move).unwrap(),
                nid: child,
            };
            self.nodes.get_mut(nid).pv = m;
        }
        Some(root)
    }

    fn expand(&mut self, nid: NodeID, children: Vec<dfpn::Child>) {
        self.stats.expanded += 1;

        let mut last_child = NodeID::none();
        let ref node = self.nodes.get(nid);
        /*
        match cursor.pos.game_state() {
            game::BoardState::InPlay => (),
            _ => debug_assert!(
                false,
                format!("expanding a terminal node! {:?}", node.bounds)
            ),
        };
        */
        debug_assert!(!node.flag(FLAG_EXPANDED));
        for ch in children.iter() {
            let mut alloc = self.nodes.alloc();
            alloc.parent = nid;
            if !node.flag(FLAG_AND) {
                alloc.set_flag(FLAG_AND)
            }
            alloc.r#move = ch.r#move;
            alloc.pv = ch.entry.pv;
            alloc.bounds = ch.entry.bounds;
            alloc.vbounds = ch.entry.bounds;

            // self.evaluate(&mut alloc, &child_pos);

            alloc.sibling = last_child;
            last_child = alloc.id;
            if alloc.bounds.delta == 0 {
                break;
            }
        }
        {
            let ref mut node_mut = self.nodes.get_mut(nid);
            node_mut.set_flag(FLAG_EXPANDED);
            node_mut.first_child = last_child;
        }
    }

    fn update_ancestors(&mut self, mut nid: NodeID) -> NodeID {
        debug_assert!(nid.exists());
        loop {
            let ref node = self.nodes.get(nid);
            debug_assert!(
                node.flag(FLAG_EXPANDED),
                "expected expanded node: {:?}",
                nid
            );
            let old = (node.bounds, node.vbounds);
            let node_mut = self.set_proof_numbers(nid);
            if old == (node_mut.bounds, node_mut.vbounds) {
                return nid;
            }
            let mut free_child = NodeID::none();
            if node_mut.bounds.solved() && node_mut.vcount == 0 {
                free_child = node_mut.first_child;
                node_mut.first_child = NodeID::none();
            }
            if free_child.exists() {
                self.free_siblings(free_child);
            }
            {
                let ref node = self.nodes.get(nid);
                if node.proof() == 0 {
                    self.stats.proved += 1;
                } else if node.disproof() == 0 {
                    self.stats.disproved += 1;
                }
                if node.parent.exists() {
                    nid = node.parent;
                } else {
                    return nid;
                }
            }
        }
    }

    fn free_siblings(&mut self, child: NodeID) {
        let mut todo = vec![child];
        while let Some(it) = todo.pop() {
            let ref node = self.nodes.get(it);
            if node.sibling.exists() {
                todo.push(node.sibling);
            }
            if node.first_child.exists() {
                todo.push(node.first_child);
            }
            self.nodes.free(it);
        }
    }

    fn player(&self) -> game::Player {
        self.position.player()
    }

    fn depth(&self, mut nid: NodeID) -> usize {
        let mut depth = 0;
        while nid.exists() {
            depth += 1;
            nid = self.nodes.get(nid).parent;
        }
        depth
    }
}
