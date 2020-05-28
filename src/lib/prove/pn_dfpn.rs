use crate::game;
use crate::minimax;
use crate::prove::dfpn;
use crate::prove::node_pool;
use crate::prove::node_pool::{NodeID, Pool};
use crate::prove::{Bounds, Evaluation, INFINITY};
use crate::table;
use crate::util;

use crossbeam;
use crossbeam::channel;
use hdrhistogram::Histogram;
use serde::Serialize;

use std::cmp::min;
use std::mem;
use std::mem::MaybeUninit;
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
            mid: Default::default(),
            worker: Default::default(),
        }
    }
}

const FLAG_EXPANDED: u16 = 1 << 0;
const FLAG_AND: u16 = 1 << 1;
const FLAG_FREE: u16 = 1 << 2;

struct Node {
    parent: NodeID,

    bounds: Bounds,
    vbounds: Bounds,

    value: Evaluation,
    r#move: game::Move,
    pos: game::Game,
    flags: u16,
    first_child: NodeID,
    sibling: NodeID,
    work: u64,
}

impl Node {
    fn flag(&self, flag: u16) -> bool {
        (self.flags & flag) != 0
    }

    fn set_flag(&mut self, flag: u16) {
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
            pos: MaybeUninit::zeroed().assume_init(),
            bounds: Bounds::unity(),
            vbounds: Bounds::unity(),
            value: Evaluation::Unknown,
            flags: FLAG_FREE,
            first_child: NodeID::none(),
            sibling: NodeID::none(),
            work: 0,
        }));
    }

    fn alloc(&mut self) {
        debug_assert!(self.flag(FLAG_FREE));
        debug_assert!(!self.first_child.exists());
        self.parent = NodeID::none();
        self.bounds = Bounds { phi: 0, delta: 0 };
        self.vbounds = Bounds { phi: 0, delta: 0 };
        self.value = Evaluation::Unknown;
        self.flags = 0;
    }

    fn free(&mut self) {
        debug_assert!(!self.flag(FLAG_FREE));
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
    tick: Instant,
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

fn hdrmerge<T: hdrhistogram::Counter>(l: &Histogram<T>, r: &Histogram<T>) -> Histogram<T> {
    let mut out = l.clone();
    out.add(r).unwrap();
    out
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
            work_per_job: hdrmerge(&self.work_per_job, &rhs.work_per_job),
            us_per_job: hdrmerge(&self.us_per_job, &rhs.us_per_job),
        }
    }
}

struct Worker<'a, Table, Probe>
where
    Table: table::Table<dfpn::Entry>,
    Probe: FnMut(&dfpn::Stats, &dfpn::Entry, &Vec<dfpn::Child>),
{
    cfg: &'a Config,
    jobs: crossbeam::Receiver<Job>,
    results: crossbeam::Sender<JobResult>,
    mid: dfpn::MID<'a, Table, Probe>,
    stats: WorkerStats,
}

macro_rules! probe_label {
    ($label:ident) => {
        unsafe {
            asm!(concat!(
                "probe_",
                stringify!($label),
                ".${:uid}",
                ":\n",
                ".globl probe_",
                stringify!($label),
                ".${:uid}",
                "\n"
            ) : : : : "volatile");
        }
    };
}

impl<'a, Table, Probe> Worker<'a, Table, Probe>
where
    Table: table::Table<dfpn::Entry>,
    Probe: FnMut(&dfpn::Stats, &dfpn::Entry, &Vec<dfpn::Child>),
{
    fn run(&mut self) {
        for job in self.jobs.iter() {
            probe_label!(enter_run_job);
            let start = Instant::now();
            let (entry, work) = self.mid.mid(
                job.bounds,
                self.cfg.split_threshold,
                dfpn::Entry {
                    hash: job.pos.zobrist(),
                    bounds: job.node,
                    child: 0xff,
                    pv: game::Move::none(),
                    work: 0,
                },
                &job.pos,
            );
            let t = Instant::now().duration_since(start);
            self.stats.us_per_job.record(t.as_micros() as u64).unwrap();
            self.stats.work_per_job.record(work).unwrap();
            probe_label!(exit_run_job);

            if let Err(_) = self.results.send(JobResult {
                nid: job.nid,
                work: work,
                bounds: entry.bounds,
            }) {
                break;
            }
        }
    }
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
            tick: start,
            limit: cfg.timeout.map(|t| Instant::now() + t),
            position: pos.clone(),
            root: NodeID::none(),
        };

        {
            let mut root = prover.nodes.alloc();

            root.parent = NodeID::none();
            root.pos = pos.clone();
            prover.root = root.id;
            prover.evaluate(&mut root);
        }

        let table = table::ConcurrentTranspositionTable::<dfpn::Entry, typenum::U4>::with_memory(
            cfg.dfpn.table_size,
        );

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
                            probe: |_, _, _| {},
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

            prover.search(job_send, res_recv, prover.root, prover.cfg.max_nodes);

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
                _ => root.value,
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
            self.cfg.dfpn.threads
        }
    }
    fn evaluate(&self, node: &mut node_pool::AllocedNode<Node>) {
        let player = self.player();
        let res = node.pos.game_state();
        node.value = match res {
            game::BoardState::InPlay => Evaluation::Unknown,
            game::BoardState::Drawn => {
                if node.pos.player() == player {
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
        let bounds = match node.value {
            Evaluation::True | Evaluation::False => {
                if node.flag(FLAG_AND) == (node.value == Evaluation::True) {
                    Bounds::losing()
                } else {
                    Bounds::winning()
                }
            }
            Evaluation::Unknown => {
                Bounds::unity()
                // delta = self.cursor.position(nid).all_moves().count() as u32;
                // delta = self.cursor.position(nid).bound_depth() as u32;
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

    fn calc_proof_numbers(&self, _nid: NodeID, node: &Node) -> (Bounds, Bounds, u64) {
        debug_assert!(node.flag(FLAG_EXPANDED));
        let mut work = 0;
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
            c = node.sibling;
            work += node.work;
        }
        (bounds, vbounds, work)
    }

    fn search(
        &mut self,
        jobs: channel::Sender<Job>,
        results: channel::Receiver<JobResult>,
        root: NodeID,
        limit: Option<usize>,
    ) {
        let mut inflight = 0;
        while {
            let ref nd = self.nodes.get(root);
            !nd.bounds.solved()
        } {
            let now = Instant::now();
            if now > self.tick && self.cfg.debug > 0 {
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
                self.tick = now + TICK_INTERVAL;
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
                probe_label!(enter_select_job);
                let candidate = self.select_job();
                probe_label!(exit_select_job);

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
        let (mut mpn, mut bounds) = self.select_most_proving(self.root, Bounds::root())?;
        let mut did_split = false;
        if self.nodes.get(mpn).work >= self.cfg.split_threshold {
            self.expand(mpn);
            let (mpn2, bounds2) = self.select_most_proving(mpn, bounds)?;

            mpn = mpn2;
            bounds = bounds2;
            did_split = true;
        }

        let node = self.nodes.get_mut(mpn);
        if node.vbounds.exceeded(bounds) {
            return None;
        }

        if self.cfg.debug > 2 {
            println!(
                "Select mpn nid={:?} bounds={:?} node={:?} vnode={:?} work={} split={}",
                mpn, bounds, node.bounds, node.vbounds, node.work, did_split,
            )
        }

        let job = Job {
            nid: mpn,
            node: node.bounds,
            bounds: bounds,
            pos: node.pos.clone(),
        };
        node.vbounds = if node.bounds.phi < node.bounds.delta {
            Bounds::winning()
        } else {
            Bounds::losing()
        };
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
        debug_assert!(!node.flag(FLAG_EXPANDED));
        node.bounds = res.bounds;
        node.vbounds = res.bounds;
        node.work += res.work;
        let parent = node.parent;
        if parent.exists() {
            self.update_ancestors(parent);
        }
    }

    fn select_most_proving(
        &mut self,
        mut nid: NodeID,
        mut bounds: Bounds,
    ) -> Option<(NodeID, Bounds)> {
        while self.nodes.get(nid).flag(FLAG_EXPANDED) {
            let ref node = self.nodes.get(nid);
            if node.vbounds.solved() {
                return None;
            }
            debug_assert!(
                node.bounds.phi != 0,
                "select mpn phi=0, node={:?} vnode={:?} depth={}",
                node.bounds,
                node.vbounds,
                self.depth(nid),
            );
            if self.cfg.dfpn.threads == 1 {
                debug_assert!(
                    node.bounds == node.vbounds,
                    "single-threaded inconsistent bounds={:?} vbounds={:?} d={}",
                    node.bounds,
                    node.vbounds,
                    self.depth(nid)
                );
            }
            let mut child = NodeID::none();
            let (mut delta_1, mut delta_2) = (INFINITY, INFINITY);
            let mut c = node.first_child;
            while c.exists() {
                let ch = self.nodes.get(c);
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
            let cn = self.nodes.get(child);
            bounds = Bounds {
                phi: bounds.delta + cn.vbounds.phi - node.vbounds.delta,
                delta: min(bounds.phi, delta_2 + 1),
            };
            nid = child;
        }
        Some((nid, bounds))
    }

    fn expand(&mut self, nid: NodeID) {
        self.stats.expanded += 1;

        let mut last_child = NodeID::none();
        let ref node = self.nodes.get(nid);
        match node.pos.game_state() {
            game::BoardState::InPlay => (),
            _ => debug_assert!(
                false,
                format!("expanding a terminal node! {:?}", node.bounds)
            ),
        };
        debug_assert!(!node.flag(FLAG_EXPANDED));
        let pos = node.pos.clone();
        for m in pos.all_moves() {
            let mut alloc = self.nodes.alloc();
            alloc.parent = nid;
            if !node.flag(FLAG_AND) {
                alloc.set_flag(FLAG_AND)
            }
            alloc.pos = pos.make_move(m).unwrap();
            alloc.r#move = m;
            self.evaluate(&mut alloc);
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
            let (bounds, vbounds, work) = self.calc_proof_numbers(nid, node);
            if bounds == node.bounds && vbounds == node.vbounds {
                return nid;
            }
            /*
            let mut free_child = NodeID::none();
            */
            {
                let ref mut node_mut = self.nodes.get_mut(nid);
                node_mut.bounds = bounds;
                node_mut.vbounds = vbounds;
                node_mut.work = work;
                /*
                if bounds.solved() {
                    free_child = node_mut.first_child;
                    node_mut.first_child = NodeID::none();
                }
                */
            }
            /*
            if free_child.exists() {
                self.free_siblings(free_child);
            }
            */
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
