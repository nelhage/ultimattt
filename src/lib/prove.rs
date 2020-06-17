pub mod dfpn;
pub mod node_pool;
pub mod pn;
pub mod pn_dfpn;
pub mod spdfpn;

use crate::game;

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

    pub fn root() -> Self {
        Bounds {
            phi: INFINITY / 2,
            delta: INFINITY / 2,
        }
    }

    pub fn exceeded(&self, other: Bounds) -> bool {
        self.phi >= other.phi || self.delta >= other.delta
    }

    pub fn solved(&self) -> bool {
        self.phi == 0 || self.delta == 0
    }
}

const MAYBE_X: u8 = 0b0001;
const MAYBE_O: u8 = 0b0010;
const MAYBE_DRAW: u8 = 0b0100;
const VALID_BITS: u8 = 0b0111;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Status(u8);

impl Default for Status {
    fn default() -> Self {
        Status::unproven()
    }
}

impl Status {
    pub const fn unproven() -> Self {
        Status(MAYBE_X | MAYBE_O | MAYBE_DRAW)
    }

    pub const fn x() -> Self {
        Status(MAYBE_X)
    }

    pub const fn o() -> Self {
        Status(MAYBE_O)
    }

    pub const fn draw() -> Self {
        Status(MAYBE_DRAW)
    }

    pub const fn draw_or_x() -> Self {
        Status(MAYBE_DRAW | MAYBE_X)
    }

    pub const fn draw_or_o() -> Self {
        Status(MAYBE_DRAW | MAYBE_O)
    }

    pub fn for_player(player: game::Player) -> Self {
        match player {
            game::Player::X => Self::x(),
            game::Player::O => Self::o(),
        }
    }

    pub fn from_byte(v: u8) -> Option<Self> {
        if v & !VALID_BITS != 0 {
            return None;
        }
        if v == (MAYBE_X | MAYBE_O) || v == 0 {
            return None;
        }
        Some(Self(v))
    }

    pub fn as_byte(self) -> u8 {
        self.0
    }

    pub fn merge(self, other: Self) -> Option<Self> {
        Self::from_byte(self.0 & other.0)
    }

    pub fn is_winnable(self, who: game::Player) -> bool {
        match who {
            game::Player::X => self.0 & MAYBE_X != 0,
            game::Player::O => self.0 & MAYBE_O != 0,
        }
    }

    pub fn is_won(self, who: game::Player) -> bool {
        match who {
            game::Player::X => self.0 == MAYBE_X,
            game::Player::O => self.0 == MAYBE_O,
        }
    }
}
