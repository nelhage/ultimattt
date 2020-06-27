use crate::game;
use crate::minimax;
use crate::progress::Ticker;
use crate::prove;
use crate::prove::dfpn;
use crate::prove::dfpn::{Child, Config, Entry, Probe, Stats};
use crate::prove::{Bounds, INFINITY};
use crate::table;

use parking_lot::{Condvar, Mutex, MutexGuard, RwLock};
use std::collections::hash_map::HashMap;
use std::ops::{Deref, DerefMut};
use std::time::{Duration, Instant};

struct SharedState {
    vtable: HashMap<u64, VEntry>,
    shutdown: bool,
    start: Instant,
    tick: Ticker,
    dump_tick: Ticker,
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

struct SPDFPNWorker<'a, P>
where
    P: dfpn::ProbeFn,
{
    mid: dfpn::MID<'a, table::ConcurrentTranspositionTableHandle<'a, Entry, 4>, P>,
    guard: YieldableGuard<'a, SharedState>,
    wait: &'a Condvar,
}

impl<Probe> SPDFPNWorker<'_, Probe>
where
    Probe: dfpn::ProbeFn,
{
    fn try_run_job(
        &mut self,
        bounds: Bounds,
        pos: &game::Game,
        mut data: Entry,
        mut vdata: &mut VPath,
    ) -> (Entry, Bounds, u64, bool) {
        if self.mid.cfg.debug > 4 {
            eprintln!(
                "{:depth$}[{}]try_run_job: m={} d={depth} bounds=({}, {}) node=({}, {}) vnode=({}, {}) w={}",
                "",
                self.mid.id,
                self.mid.stack
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

        self.mid.stats.try_calls += 1;

        let mut local_work = 0;

        if data.work < self.mid.cfg.max_work_per_job {
            let vnode = self.vadd(&vdata.entry);
            if data.bounds.phi < data.bounds.delta {
                // TODO: does < / <= matter here?
                vnode.entry.bounds = Bounds::winning();
            } else {
                vnode.entry.bounds = Bounds::losing();
            }

            self.update_parents(vdata.parent);
            self.mid.stats.jobs += 1;
            if self.mid.cfg.debug > 3 {
                eprintln!(
                    "{:2$}[{}]try_run_job: mid[d={}]({}, {}) work={}",
                    "",
                    self.mid.id,
                    vdata.depth(),
                    bounds.phi,
                    bounds.delta,
                    data.work,
                );
            }
            self.guard.drop_lock();
            let (result, local_work, _) =
                self.mid
                    .mid(bounds, self.mid.cfg.max_work_per_job, data, pos);
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
            let entry = self.mid.ttlookup_or_default(&g, prove::Status::unproven());
            let child = Child {
                position: g,
                r#move: m,
                entry: entry,
            };
            let vchild = self
                .guard
                .vtable
                .get(&child.entry.hash)
                .map(|ve| Child {
                    entry: ve.entry.clone(),
                    r#move: m,
                    position: child.position.clone(),
                })
                .unwrap_or_else(|| child.clone());

            vdata.children.push(vchild);
            children.push(child);
        }
        debug_assert_eq!(vdata.children.len(), children.len());

        let mut did_job = false;
        loop {
            if did_job {
                for (i, child) in children.iter_mut().enumerate() {
                    if let Some(e) = self.mid.table.lookup(child.position.zobrist()) {
                        child.entry = e;
                    }
                    if let Some(v) = self.guard.vtable.get(&child.position.zobrist()) {
                        vdata.children[i].entry = v.entry.clone();
                    } else {
                        vdata.children[i].entry = child.entry.clone();
                    }
                }
            }
            data.bounds = dfpn::compute_bounds(&children);
            vdata.entry.bounds = dfpn::compute_bounds(&vdata.children);
            dfpn::populate_pv(&mut data, &children);

            (self.mid.probe)(&self.mid.stats, &pos, &data, &children);

            if self.mid.cfg.debug > 5 {
                eprintln!(
                    "{0:1$}[{2}]try_run_job[loop]: node=({3}, {4}) vnode=({5}, {6}) w={7}",
                    "",
                    self.mid.stack.len(),
                    self.mid.id,
                    data.bounds.phi,
                    data.bounds.delta,
                    vdata.entry.bounds.phi,
                    vdata.entry.bounds.delta,
                    data.work,
                );
            }

            self.mid.table.store(&data);
            if vdata.entry.bounds.exceeded(bounds) || did_job {
                break;
            }
            let (idx, child_bounds) = dfpn::select_child(
                &vdata.children,
                bounds,
                &mut vdata.entry,
                self.mid.cfg.epsilon,
            );
            let child_data = vdata.children[idx].entry.clone();
            let mut vchild = VPath {
                parent: Some(vdata as *mut VPath),
                children: Vec::new(),
                entry: child_data,
            };

            self.mid.stack.push(children[idx].r#move);
            let (child_result, child_vbounds, child_work, ran) = self.try_run_job(
                child_bounds,
                &children[idx].position,
                children[idx].entry.clone(),
                &mut vchild,
            );
            self.mid.stack.pop();
            did_job = ran;

            local_work += child_work;
            data.work += child_work;

            children[idx].entry = child_result;
            vdata.children[idx].entry.bounds = child_vbounds;
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
            let mut root = self.mid.table.lookup(pos.zobrist()).unwrap_or(Entry {
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

            if self.mid.cfg.threads == 1 {
                debug_assert!(self.guard.vtable.is_empty(), "leaking ventries");
                debug_assert!(root.bounds == vroot.entry.bounds, "vroot differs from root");
                debug_assert!(
                    did_job || root.bounds.solved(),
                    "single-threaded no progress"
                );
            }

            if self.mid.cfg.debug > 2 && did_job {
                eprintln!(
                    "[{}] top root=({},{}) vroot=({},{}) work={}",
                    self.mid.id,
                    root.bounds.phi,
                    root.bounds.delta,
                    vroot.entry.bounds.phi,
                    vroot.entry.bounds.delta,
                    work,
                );
            }

            let now = Instant::now();
            if self.mid.cfg.debug > 0 && self.guard.tick.tick() {
                let elapsed = now.duration_since(self.guard.start);
                eprintln!(
                    "[{}]t={}.{:03}s root=({},{}) jobs={} work={}",
                    self.mid.id,
                    elapsed.as_secs(),
                    elapsed.subsec_millis(),
                    root.bounds.phi,
                    root.bounds.delta,
                    self.mid.stats.jobs,
                    work,
                );
            }

            if let Some(_) = self.mid.cfg.dump_table {
                if self.guard.dump_tick.tick() {
                    dfpn::dump_table(&self.mid.cfg, &self.mid.table).expect("dump_table failed");
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
            self.vadd(&v.entry).entry.bounds = dfpn::compute_bounds(&v.children);
            node = v.parent;
        }
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

pub(in crate::prove) fn run(
    start: Instant,
    cfg: &Config,
    root: &game::Game,
    probe: Option<Probe>,
) -> (Stats, Entry, Vec<game::Move>, u64) {
    let mmcfg = minimax::Config {
        max_depth: Some(cfg.minimax_cutoff as i64 + 1),
        timeout: Some(Duration::from_secs(1)),
        debug: if cfg.debug > 6 { 1 } else { 0 },
        table_bytes: None,
        draw_winner: Some(root.player().other()),
    };
    let table = if let Some(ref path) = cfg.load_table {
        table::ConcurrentTranspositionTable::from_file(path).expect("invalid table file")
    } else {
        table::ConcurrentTranspositionTable::with_memory(cfg.table_size)
    };
    let shared = Mutex::new(SharedState {
        vtable: HashMap::new(),
        shutdown: false,
        start: start,
        tick: Ticker::new(dfpn::TICK_TIME),
        dump_tick: Ticker::new(cfg.dump_interval),
    });
    let cv = Condvar::new();

    let mut rwlock: Option<RwLock<Probe>> = None;
    let probe = probe.map(|p| {
        rwlock = Some(RwLock::new(p));
        rwlock.as_ref().unwrap()
    });
    let (work, mut stats) = crossbeam::scope(|s| {
        let mut guards = Vec::new();
        for i in 0..cfg.threads {
            let player = root.player();
            let root = root;
            let cfg = cfg;
            let table = table.handle();
            let cref = &cv;
            let sref = &shared;
            let probe = probe.clone();
            let mcref = &mmcfg;
            guards.push(
                s.builder()
                    .name(format!("worker-{}", i))
                    .spawn(move |_| {
                        let mut worker = SPDFPNWorker {
                            mid: dfpn::MID {
                                id: i,
                                cfg: cfg,
                                player: player,
                                table: table,
                                stack: Vec::new(),
                                minimax: minimax::Minimax::with_config(mcref),
                                stats: Default::default(),
                                probe:
                                    |stats: &Stats,
                                     pos: &game::Game,
                                     data: &Entry,
                                     children: &Vec<Child>| {
                                        if let Some(ref p) = probe {
                                            let now: Instant;
                                            {
                                                let r = p.read();
                                                if data.hash != r.hash {
                                                    return;
                                                }
                                                now = Instant::now();
                                                if now < r.tick && !data.bounds.solved() {
                                                    return;
                                                }
                                            }

                                            let mut w = p.write();
                                            w.do_probe(
                                                now + Duration::from_millis(10),
                                                stats.mid,
                                                pos,
                                                data,
                                                children,
                                            );
                                        }
                                    },
                            },
                            guard: YieldableGuard::new(sref),
                            wait: cref,
                        };

                        (worker.run(root), worker.mid.stats.clone())
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

    let pv = dfpn::extract_pv(cfg, &mut table.handle(), root);
    dfpn::dump_table(cfg, &table.handle()).expect("final dump_table");
    stats.tt = table.stats();
    let root = table
        .lookup(&mut stats.tt, root.zobrist())
        .expect("no entry for root");
    (stats, root, pv, work)
}
