use crate::game;
extern crate bytesize;

mod node_pool;

use node_pool::{NodeID, Pool};

use bytesize::ByteSize;
use std::cmp::min;
use std::mem;
use std::mem::MaybeUninit;
use std::path::Path;
use std::time::{Duration, Instant};

#[derive(Clone, Debug)]
pub struct Config {
    pub debug: usize,
    pub max_nodes: Option<u64>,
    pub timeout: Option<Duration>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            debug: 0,
            max_nodes: None,
            timeout: None,
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct Stats {
    pub proved: usize,
    pub disproved: usize,
    pub expanded: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u16)]
pub enum Evaluation {
    True,
    False,
    Unknown,
}

const FLAG_EXPANDED: u16 = 1 << 0;
const FLAG_AND: u16 = 1 << 1;
const FLAG_FREE: u16 = 1 << 2;

struct Node {
    parent: NodeID,
    pos: game::Game,

    phi: u32,
    delta: u32,

    value: Evaluation,
    flags: u16,
    first_child: NodeID,
    sibling: NodeID,
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
            self.delta
        } else {
            self.phi
        }
    }

    fn disproof(&self) -> u32 {
        if self.flag(FLAG_AND) {
            self.phi
        } else {
            self.delta
        }
    }
}

impl node_pool::Node for Node {
    unsafe fn init(ptr: *mut MaybeUninit<Node>, free: NodeID) {
        ptr.write(MaybeUninit::new(Node {
            parent: free,
            pos: MaybeUninit::zeroed().assume_init(),
            phi: 0,
            delta: 0,
            value: Evaluation::Unknown,
            flags: FLAG_FREE,
            first_child: NodeID::none(),
            sibling: NodeID::none(),
        }));
    }

    fn alloc(&mut self) {
        debug_assert!(self.flag(FLAG_FREE));
        debug_assert!(!self.first_child.exists());
        self.parent = NodeID::none();
        // self.pos = pos.clone();
        self.phi = 0;
        self.delta = 0;
        self.value = Evaluation::Unknown;
        self.flags = 0;
    }

    fn free(&mut self) {
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
    config: Config,
    stats: Stats,
    nodes: Pool<Node>,

    start: Instant,
    tick: Instant,
    limit: Option<Instant>,

    position: game::Game,
    root: NodeID,
}

pub struct ProofResult {
    pub result: Evaluation,
    pub duration: Duration,
    pub proof: u32,
    pub disproof: u32,

    pub allocated: usize,
    pub stats: Stats,
}

const PROGRESS_INTERVAL: isize = 10000;
static TICK_INTERVAL: Duration = Duration::from_millis(100);

impl Prover {
    pub fn prove(cfg: &Config, pos: &game::Game) -> ProofResult {
        if cfg.debug > 1 {
            eprintln!("Entering prove() sizeof(Node)={}", mem::size_of::<Node>());
        }
        let start = Instant::now();
        let mut prover = Prover {
            config: cfg.clone(),
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
            prover.evaluate(&mut *root);
            prover.set_proof_numbers(&mut *root);
        }
        prover.search(prover.root);

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
            allocated: prover.nodes.stats.allocated.get(),
        }
    }

    fn evaluate(&self, node: &mut Node) {
        let player = self.player();
        let res = node.pos.game_state();
        node.value = match res {
            game::BoardState::InPlay => Evaluation::Unknown,
            game::BoardState::Drawn => Evaluation::False,
            game::BoardState::Won(p) => {
                if p == player {
                    Evaluation::True
                } else {
                    Evaluation::False
                }
            }
        }
    }

    fn set_proof_numbers(&self, node: &mut Node) {
        let (phi, delta) = self.calc_proof_numbers(node);
        node.phi = phi;
        node.delta = delta;
    }

    fn calc_proof_numbers(&self, node: &Node) -> (u32, u32) {
        let mut phi: u32;
        let mut delta: u32;
        if node.flag(FLAG_EXPANDED) {
            delta = 0;
            phi = std::u32::MAX;
            let mut c = node.first_child;
            while c.exists() {
                let node = self.nodes.get(c);
                delta = delta.saturating_add(node.phi);
                phi = min(phi, node.delta);
                c = node.sibling;
            }
        } else {
            match node.value {
                Evaluation::True | Evaluation::False => {
                    if node.flag(FLAG_AND) == (node.value == Evaluation::True) {
                        phi = std::u32::MAX;
                        delta = 0;
                    } else {
                        phi = 0;
                        delta = std::u32::MAX;
                    }
                }
                Evaluation::Unknown => {
                    phi = 1;
                    delta = 1;
                }
            }
        }
        (phi, delta)
    }

    fn search(&mut self, root: NodeID) {
        let mut current = root;
        let mut i = 0;
        while {
            let ref nd = self.nodes.get(root);
            nd.phi != 0 && nd.delta != 0
        } {
            let next = self.select_most_proving(current);
            self.expand(next);
            current = self.update_ancestors(next);
            i += 1;
            if i % PROGRESS_INTERVAL == 0 {
                let now = Instant::now();
                if now > self.tick && self.config.debug > 0 {
                    let elapsed = now.duration_since(self.start);
                    eprintln!(
                        "t={}.{:03}s nodes={}/{} proved={} disproved={} root=({}, {}) n/ms={:.0} rss={}",
                        elapsed.as_secs(),
                        elapsed.subsec_millis(),
                        self.nodes.stats.live(),
                        self.nodes.stats.allocated.get(),
                        self.stats.proved,
                        self.stats.disproved,
                        self.nodes.get(self.root).proof(),
                        self.nodes.get(self.root).disproof(),
                        (self.nodes.stats.allocated.get() as f64) / (elapsed.as_millis() as f64),
                        read_rss(),
                    );
                    self.tick = now + TICK_INTERVAL;
                }
                if let Some(limit) = self.limit {
                    if now > limit {
                        break;
                    }
                }
            }
        }
    }

    fn select_most_proving(&self, mut nid: NodeID) -> NodeID {
        while self.nodes.get(nid).flag(FLAG_EXPANDED) {
            let ref node = self.nodes.get(nid);
            debug_assert!(
                node.phi != 0,
                format!(
                    "expand phi=0, root=({}, {}), depth={}",
                    self.nodes.get(self.root).proof(),
                    self.nodes.get(self.root).disproof(),
                    self.depth(self.root),
                )
            );
            let mut child = NodeID::none();
            let mut c = node.first_child;
            while c.exists() {
                let ch = self.nodes.get(c);
                if ch.delta == node.phi {
                    child = c;
                    break;
                }
                c = ch.sibling;
            }
            debug_assert!(child.exists(), "found a child");
            nid = child;
        }
        nid
    }

    fn expand(&mut self, nid: NodeID) {
        self.stats.expanded += 1;
        let mut last_child = NodeID::none();
        let ref node = self.nodes.get(nid);
        match node.pos.game_state() {
            game::BoardState::InPlay => (),
            _ => debug_assert!(
                false,
                format!("expanding a terminal node! ({}, {})", node.phi, node.delta)
            ),
        };
        debug_assert!(!node.flag(FLAG_EXPANDED));
        for m in node.pos.all_moves() {
            let child_pos = node
                .pos
                .make_move(m)
                .expect("all_moves() returned valid move");
            let mut alloc = self.nodes.alloc();
            alloc.parent = nid;
            if !node.flag(FLAG_AND) {
                alloc.set_flag(FLAG_AND)
            }
            alloc.pos = child_pos.clone();
            self.evaluate(&mut *alloc);
            self.set_proof_numbers(&mut *alloc);
            alloc.sibling = last_child;
            last_child = alloc.id;
            if alloc.delta == 0 {
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
        loop {
            let ref node = self.nodes.get(nid);
            debug_assert!(node.flag(FLAG_EXPANDED));
            let (oldphi, olddelta) = (node.phi, node.delta);
            let (phi, delta) = self.calc_proof_numbers(node);
            if phi == oldphi && delta == olddelta {
                return nid;
            }
            let mut free_child = NodeID::none();
            {
                let ref mut node_mut = self.nodes.get_mut(nid);
                node_mut.phi = phi;
                node_mut.delta = delta;
                if phi == 0 || delta == 0 {
                    free_child = node_mut.first_child;
                    node_mut.first_child = NodeID::none();
                }
            }
            while free_child.exists() {
                self.free_tree(free_child);
                free_child = self.nodes.get(free_child).sibling;
            }
            {
                let ref node = self.nodes.get(nid);
                if node.proof() == 0 {
                    self.stats.proved += 1;
                } else if node.disproof() == 0 {
                    self.stats.disproved += 1;
                }
                if node.parent.exists() {
                    nid = node.parent
                } else {
                    return nid;
                }
            }
        }
    }

    fn free_tree(&mut self, nid: NodeID) {
        self.nodes.free(nid);
        let mut child = self.nodes.get(nid).first_child;
        while child.exists() {
            let next = self.nodes.get(child).sibling;
            self.free_tree(child);
            child = next;
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

fn read_rss() -> ByteSize {
    let path = Path::new("/proc/")
        .join(std::process::id().to_string())
        .join("stat");
    let stat = std::fs::read_to_string(path).unwrap();
    let mut bits = stat.split(' ');
    ByteSize::kib(4 * bits.nth(23).unwrap().parse::<u64>().unwrap())
}
