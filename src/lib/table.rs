use typenum;

use std::marker::PhantomData;
use std::mem;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;

#[derive(Default)]
struct AtomicStats {
    lookups: AtomicUsize,
    hits: AtomicUsize,
    stores: AtomicUsize,
}

impl AtomicStats {
    fn load(&self) -> Stats {
        Stats {
            lookups: self.lookups.load(Ordering::SeqCst),
            hits: self.hits.load(Ordering::SeqCst),
            stores: self.stores.load(Ordering::SeqCst),
        }
    }
}

pub struct Stats {
    pub lookups: usize,
    pub hits: usize,
    pub stores: usize,
}

pub struct TranspositionTable<E, N>
where
    E: Entry + Default + Clone,
    N: typenum::Unsigned,
{
    index: Box<[u8]>,
    entries: Box<[E]>,
    stats: AtomicStats,
    n: PhantomData<N>,
}

pub trait Entry {
    fn hash(&self) -> u64;
    fn valid(&self) -> bool;
    fn better_than(&self, rhs: &Self) -> bool;
}

pub const DEFAULT_TABLE_SIZE: usize = 1 << 30;

fn new_default_slice<T>(len: usize) -> Box<[T]>
where
    T: Default,
{
    let mut slice = Box::new_uninit_slice(len);
    for e in slice.iter_mut() {
        *e = MaybeUninit::new(Default::default());
    }
    unsafe { slice.assume_init() }
}

impl<E, N> TranspositionTable<E, N>
where
    E: Entry + Default + Clone,
    N: typenum::Unsigned,
{
    pub fn new() -> Self {
        Self::with_memory(DEFAULT_TABLE_SIZE)
    }

    pub fn with_memory(bytes: usize) -> Self {
        let len = bytes / (1 + mem::size_of::<E>());

        TranspositionTable::<E, N> {
            index: new_default_slice(len),
            entries: new_default_slice(len),
            stats: Default::default(),
            n: PhantomData,
        }
    }

    pub fn lookup(&self, h: u64) -> Option<E> {
        self.stats.lookups.fetch_add(1, Ordering::Relaxed);
        let base = h as usize;
        for j in 0..N::to_usize() {
            let i = (base + j) % self.entries.len();
            if self.index[i] != (h & 0xff) as u8 {
                continue;
            }
            if self.entries[i].valid() && self.entries[i].hash() == h {
                self.stats.hits.fetch_add(1, Ordering::Relaxed);
                return Some(self.entries[i].clone());
            }
        }
        None
    }

    pub fn store(&mut self, ent: &E) -> bool {
        debug_assert!(ent.valid());
        let mut worst: Option<usize> = None;
        let base = ent.hash() as usize;
        for j in 0..N::to_usize() {
            let i = (base + j) % self.entries.len();
            if !self.entries[i].valid() || self.entries[i].hash() == ent.hash() {
                worst = Some(i);
                break;
            } else if let Some(w) = worst {
                if self.entries[w].better_than(&self.entries[i]) {
                    worst = Some(i);
                }
            } else {
                worst = Some(i);
            }
        }
        let idx = worst.unwrap();
        if !self.entries[idx].valid() || ent.better_than(&self.entries[idx]) {
            self.index[idx] = (ent.hash() & 0xff) as u8;
            self.entries[idx] = ent.clone();
            self.stats.stores.fetch_add(1, Ordering::Relaxed);
            true
        } else {
            false
        }
    }

    pub fn stats(&self) -> Stats {
        self.stats.load()
    }
}

pub struct ConcurrentTranspositionTable<E, N>(RwLock<TranspositionTable<E, N>>)
where
    E: Entry + Default + Clone,
    N: typenum::Unsigned;

impl<E, N> ConcurrentTranspositionTable<E, N>
where
    E: Entry + Default + Clone,
    N: typenum::Unsigned,
{
    pub fn new() -> Self {
        Self::with_memory(DEFAULT_TABLE_SIZE)
    }

    pub fn with_memory(bytes: usize) -> Self {
        Self(RwLock::new(TranspositionTable::with_memory(bytes)))
    }

    pub fn lookup(&self, hash: u64) -> Option<E> {
        self.0.read().unwrap().lookup(hash)
    }

    pub fn store(&self, ent: &E) -> bool {
        self.0.write().unwrap().store(ent)
    }

    pub fn stats(&self) -> Stats {
        self.0.read().unwrap().stats()
    }
}
