pub mod dfpn;
pub mod node_pool;
pub mod pn;
pub mod spdfpn;

use std::fmt;

pub const INFINITY: u32 = 1 << 31;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Evaluation {
    True,
    False,
    Unknown,
}

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
    pub fn winning() -> Self {
        Bounds {
            phi: 0,
            delta: INFINITY,
        }
    }

    pub fn losing() -> Self {
        Bounds {
            phi: INFINITY,
            delta: 0,
        }
    }

    pub fn unity() -> Self {
        Bounds { phi: 1, delta: 1 }
    }

    pub fn infinity() -> Self {
        Bounds {
            phi: INFINITY,
            delta: INFINITY,
        }
    }

    pub fn exceeded(&self, other: Bounds) -> bool {
        self.phi >= other.phi || self.delta >= other.delta
    }

    pub fn solved(&self) -> bool {
        self.phi == 0 || self.delta == 0
    }
}
