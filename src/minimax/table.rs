use crate::game;

use std::mem;
use std::mem::MaybeUninit;

pub struct TranspositionTable {
    pub entries: Box<[Entry]>,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Bound {
    Exact,
    AtLeast,
    AtMost,
}

#[derive(Clone, Debug)]
pub struct Entry {
    pub hash: u64,
    pub bound: Bound,
    pub depth: u8,
    pub pv: game::Move,
    pub value: i64,
}

pub const DEFAULT_TABLE_SIZE: usize = 1 << 30;

impl TranspositionTable {
    pub fn new() -> Self {
        Self::with_memory(DEFAULT_TABLE_SIZE)
    }

    pub fn with_memory(bytes: usize) -> Self {
        let mut entries = Box::new_uninit_slice(bytes / mem::size_of::<Entry>());
        for e in entries.iter_mut() {
            *e = MaybeUninit::zeroed();
        }

        TranspositionTable {
            entries: unsafe { entries.assume_init() },
        }
    }

    pub fn lookup(&self, h: u64) -> Option<&Entry> {
        let i = (h as usize) % self.entries.len();
        if self.entries[i].hash == h {
            Some(&self.entries[i])
        } else {
            None
        }
    }

    pub fn store(&mut self, ent: &Entry) {
        let i = (ent.hash as usize) % self.entries.len();
        if self.entries[i].depth < ent.depth {
            self.entries[i] = ent.clone();
        }
    }
}
