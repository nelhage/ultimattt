#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProofStatus(u8);

impl Default for ProofStatus {
    fn default() -> Self {
        ProofStatus::unproven()
    }
}

impl ProofStatus {
    pub fn unproven() -> Self {
        ProofStatus(MAYBE_X | MAYBE_O | MAYBE_DRAW)
    }

    pub fn x() -> Self {
        ProofStatus(MAYBE_X)
    }

    pub fn o() -> Self {
        ProofStatus(MAYBE_O)
    }

    pub fn draw() -> Self {
        ProofStatus(MAYBE_DRAW)
    }

    pub fn draw_or_x() -> Self {
        ProofStatus(MAYBE_DRAW | MAYBE_X)
    }

    pub fn draw_or_o() -> Self {
        ProofStatus(MAYBE_DRAW | MAYBE_O)
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

    pub fn can_win(self, who: game::Player) -> bool {
        match who {
            game::Player::X => self.0 & MAYBE_X != 0,
            game::Player::O => self.0 & MAYBE_O != 0,
        }
    }

    fn parse(v: &[u8]) -> Option<Self> {
        if v.len() != 1 {
            return None;
        }
        Self::from_byte(v[0])
    }
}
