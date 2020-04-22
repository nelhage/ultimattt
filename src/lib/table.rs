use typenum;

use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::RwLock;
use std::{fs, io, mem, slice};

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

#[repr(C)]
struct Header {
    version: u64,
    entries: u64,
}

const DUMPFILE_VERSION: u64 = 1;

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
        Self::with_entries(len)
    }

    pub fn with_entries(len: usize) -> Self {
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

    pub fn dump(&self, w: &mut dyn io::Write) -> io::Result<()> {
        let header = Header {
            version: DUMPFILE_VERSION,
            entries: self.entries.len() as u64,
        };
        w.write(unsafe {
            slice::from_raw_parts(
                mem::transmute::<_, *const u8>(&header),
                mem::size_of::<Header>(),
            )
        })?;
        w.write(unsafe {
            slice::from_raw_parts(
                mem::transmute::<_, *const u8>(self.index.as_ptr()),
                self.index.len() * mem::size_of_val(&self.index[0]),
            )
        })?;
        w.write(unsafe {
            slice::from_raw_parts(
                mem::transmute::<_, *const u8>(self.entries.as_ptr()),
                self.entries.len() * mem::size_of_val(&self.entries[0]),
            )
        })?;
        Ok(())
    }

    pub fn from_reader(r: &mut dyn io::Read) -> io::Result<Self> {
        let header = unsafe {
            let mut buf: MaybeUninit<Header> = MaybeUninit::uninit();
            r.read_exact(slice::from_raw_parts_mut(
                mem::transmute::<_, *mut u8>(buf.as_mut_ptr()),
                mem::size_of::<Header>(),
            ))?;
            buf.assume_init()
        };
        if header.version != DUMPFILE_VERSION {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                format!(
                    "dumpfile version mismatch: expected {} got {}",
                    DUMPFILE_VERSION, header.version,
                ),
            ));
        }
        let mut table = Self::with_entries(header.entries as usize);
        r.read_exact(&mut table.index)?;
        r.read_exact(unsafe {
            slice::from_raw_parts_mut(
                mem::transmute::<_, *mut u8>(table.entries.as_mut_ptr()),
                table.entries.len() * mem::size_of_val(&table.entries[0]),
            )
        })?;
        Ok(table)
    }

    pub fn from_file(path: &str) -> io::Result<Self> {
        let mut f = fs::File::open(path)?;
        Self::from_reader(&mut f)
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

    pub fn with_entries(len: usize) -> Self {
        Self(RwLock::new(TranspositionTable::with_entries(len)))
    }

    pub fn from_reader(r: &mut dyn io::Read) -> io::Result<Self> {
        TranspositionTable::from_reader(r).map(|t| Self(RwLock::new(t)))
    }

    pub fn from_file(path: &str) -> io::Result<Self> {
        TranspositionTable::from_file(path).map(|t| Self(RwLock::new(t)))
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

    pub fn dump(&self, w: &mut dyn io::Write) -> io::Result<()> {
        self.0.read().unwrap().dump(w)
    }
}
