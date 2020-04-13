use std::cell::{Cell, UnsafeCell};
use std::mem;
use std::mem::MaybeUninit;
use std::ops::{Deref, DerefMut};

pub trait Node
where
    Self: Sized,
{
    // Initialize a node newly requested from the OS allocator. This
    // is responsible for leaving the node in a state s.t. that it is
    // legal to call `assume_init` on the `MaybeUninit`, as well as
    // marking the node as free
    unsafe fn init(ptr: *mut MaybeUninit<Self>, free: NodeID);

    // Set up a node that is about to be allocated and returned from
    // `alloc`. This should mark a node as allocated, and do any
    // per-allocation setup.
    fn alloc(&mut self);

    // Free a node. This should mark a node as free, and tear down any
    // appropriate state.
    fn free(&mut self);

    // Nodes need to contain storage for a NodeID used as a freelist
    // pointer while the node is free. These methods provide access to
    // that pointer. These methods will only be called on nodes
    // between calls to init and alloc, or after a free and before
    // another alloc.
    fn set_free_ptr(&mut self, free: NodeID);
    fn get_free_ptr(&self) -> NodeID;
}

#[repr(transparent)]
#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub struct NodeID(u32);

impl NodeID {
    pub fn none() -> Self {
        NodeID(0xffffffff)
    }

    pub fn exists(self) -> bool {
        self.0 != 0xffffffff
    }
}

const PAGE_SIZE: usize = 1 << 20;
const PAGE_MASK: usize = PAGE_SIZE - 1;

pub struct Pool<T>
where
    T: Sized + Node,
{
    free: Cell<NodeID>,
    slabs: UnsafeCell<Vec<Box<[UnsafeCell<T>; PAGE_SIZE]>>>,
    allocating: Cell<NodeID>,
    pub stats: Stats,
}

pub struct Stats {
    pub allocated: Cell<usize>,
    pub freed: Cell<usize>,
}

impl Stats {
    pub fn live(&self) -> usize {
        self.allocated.get() - self.freed.get()
    }
}

pub struct AllocedNode<'a, T>
where
    T: Sized + Node,
{
    pub id: NodeID,
    pool: &'a Pool<T>,
    node: &'a mut T,
}

impl<'a, T> Drop for AllocedNode<'a, T>
where
    T: Sized + Node,
{
    fn drop(&mut self) {
        self.pool.allocating.set(NodeID::none());
    }
}

impl<'a, T> Deref for AllocedNode<'a, T>
where
    T: Sized + Node,
{
    type Target = T;

    fn deref(&self) -> &T {
        self.node
    }
}

impl<'a, T> DerefMut for AllocedNode<'a, T>
where
    T: Sized + Node,
{
    fn deref_mut(&mut self) -> &mut T {
        self.node
    }
}

impl<T> Pool<T>
where
    T: Sized + Node,
{
    pub fn new() -> Self {
        Self {
            free: Cell::new(NodeID::none()),
            slabs: UnsafeCell::new(Vec::new()),
            allocating: Cell::new(NodeID::none()),
            stats: Stats {
                allocated: Cell::new(0),
                freed: Cell::new(0),
            },
        }
    }

    fn new_slab(&self) {
        let ref mut slabs = unsafe { self.slabs.get().as_mut().unwrap() };
        let mut slab =
            unsafe { Box::<[UnsafeCell<MaybeUninit<T>>; PAGE_SIZE]>::new_uninit().assume_init() };

        let mut i: usize = 0;
        let base = slabs.len() * PAGE_SIZE;
        for elem in &mut slab[..] {
            let next_free = if (i + 1) == PAGE_SIZE {
                NodeID::none()
            } else {
                NodeID((base + i + 1) as u32)
            };
            let ptr = elem.get();
            unsafe {
                T::init(ptr, next_free);
            }
            i += 1;
        }

        slabs.push(unsafe { mem::transmute::<_, Box<[UnsafeCell<T>; PAGE_SIZE]>>(slab) });
        self.free.set(NodeID(base as u32));
    }

    pub fn get(&self, nd: NodeID) -> &T {
        if self.allocating.get() == nd {
            panic!(format!(
                "get(): Can't get a node while it is being allocated"
            ));
        }
        debug_assert!(nd.exists());
        unsafe {
            &*(*self.slabs.get())[(nd.0 as usize) / PAGE_SIZE][(nd.0 as usize) & PAGE_MASK].get()
        }
    }

    unsafe fn get_mut_unchecked(&self, nd: NodeID) -> &mut T {
        debug_assert!(nd.exists());
        &mut *((*self.slabs.get())[(nd.0 as usize) / PAGE_SIZE][(nd.0 as usize) & PAGE_MASK].get())
    }

    pub fn get_mut(&mut self, nd: NodeID) -> &mut T {
        if self.allocating.get().exists() {
            panic!(format!(
                "get_mut(): Can't mutate a node while allocating a node"
            ));
        }
        unsafe { self.get_mut_unchecked(nd) }
    }

    pub fn alloc(&self) -> AllocedNode<T> {
        if self.allocating.get().exists() {
            panic!(format!(
                "alloc(): Can't allocate a node while another is still allocating"
            ));
        }
        if !self.free.get().exists() {
            self.new_slab();
        }
        self.stats.allocated.set(self.stats.allocated.get() + 1);
        let out = self.free.get();
        self.free.set(self.get(out).get_free_ptr());
        self.allocating.set(out);
        let mut alloc = AllocedNode {
            pool: self,
            id: out,
            node: unsafe { self.get_mut_unchecked(out) },
        };
        alloc.alloc();
        alloc
    }

    pub fn free(&mut self, nd: NodeID) {
        if self.allocating.get() == nd {
            panic!("Can't free a node that is being allocated");
        }
        let old_free = self.free.get();
        {
            let ref mut node = self.get_mut(nd);
            node.free();
            node.set_free_ptr(old_free);
        }
        self.stats.freed.set(self.stats.freed.get() + 1);
        self.free.set(nd);
    }
}
