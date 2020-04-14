use typenum;

use std::marker::PhantomData;
use std::mem;
use std::mem::MaybeUninit;

pub struct TranspositionTable<E, N>
where
    E: Entry + Default + Clone,
    N: typenum::Unsigned,
{
    pub entries: Box<[E]>,
    n: PhantomData<N>,
}

pub trait Entry {
    fn hash(&self) -> u64;
    fn valid(&self) -> bool;
    fn better_than(&self, rhs: &Self) -> bool;
}

pub const DEFAULT_TABLE_SIZE: usize = 1 << 30;

impl<E, N> TranspositionTable<E, N>
where
    E: Entry + Default + Clone,
    N: typenum::Unsigned,
{
    pub fn new() -> Self {
        Self::with_memory(DEFAULT_TABLE_SIZE)
    }

    pub fn with_memory(bytes: usize) -> Self {
        let mut entries = Box::new_uninit_slice(bytes / mem::size_of::<E>());
        for e in entries.iter_mut() {
            *e = MaybeUninit::new(Default::default());
        }

        TranspositionTable::<E, N> {
            entries: unsafe { entries.assume_init() },
            n: PhantomData,
        }
    }

    pub fn lookup(&self, h: u64) -> Option<&E> {
        let base = h as usize;
        for j in 0..N::to_usize() {
            let i = (base + j) % self.entries.len();
            if self.entries[i].valid() && self.entries[i].hash() == h {
                return Some(&self.entries[i]);
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
        if ent.better_than(&self.entries[idx]) {
            self.entries[idx] = ent.clone();
            true
        } else {
            false
        }
    }
}
