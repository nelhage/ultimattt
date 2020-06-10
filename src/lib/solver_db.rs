#![allow(dead_code)]
use crate::game;

use rocksdb;

use std::{convert::TryInto, path::Path};

pub struct DB {
    rocks: rocksdb::DB,
}

const MAYBE_X: u8 = 0b0001;
const MAYBE_O: u8 = 0b0010;
const MAYBE_DRAW: u8 = 0b0100;
const VALID_BITS: u8 = 0b0111;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProofStatus(u8);

/*
pub const UNPROVEN: ProofStatus = ProofStatus(MAYBE_X | MAYBE_O | MAYBE_DRAW);
pub const X: ProofStatus = ProofStatus(MAYBE_X);
pub const O: ProofStatus = ProofStatus(MAYBE_O);
pub const Drawn: ProofStatus = ProofStatus(MAYBE_DRAW);
pub const MaybeX: ProofStatus = ProofStatus(MAYBE_DRAW | MAYBE_X);
pub const MaybeO: ProofStatus = ProofStatus(MAYBE_DRAW | MAYBE_O);
 */

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

/*
fn merge_proof(_key: &[u8], v: Option<&[u8]>, ops: &mut rocksdb::MergeOperands) -> Option<Vec<u8>> {
    let init = match v {
        None => Some(ProofStatus::unproven()),
        Some(bits) => ProofStatus::parse(bits),
    };
    let out = ops.fold(init, |l, r| l.and_then(|v| v.merge(ProofStatus::parse(r)?)));
    out.map(|v| vec![v.as_byte()])
}
 */
fn merge_proof(key: &[u8], v: Option<&[u8]>, ops: &mut rocksdb::MergeOperands) -> Option<Vec<u8>> {
    let init = match v {
        None => Some(ProofStatus::unproven()),
        Some(bits) => ProofStatus::parse(bits),
    };
    let out = ops.fold(init, |l, r| {
        l.and_then(|ls| {
            let rs = ProofStatus::parse(r)?;
            ls.merge(rs).or_else(|| {
                let g = DB::decode(key);
                panic!(
                    "merge error: l={:x?} r={:x?} key={:?} g={}",
                    l,
                    r,
                    key,
                    game::notation::render(&g)
                )
            })
        })
    });
    match out {
        None => panic!("key={:?} init={:?}", key, init),
        Some(v) => Some(vec![v.as_byte()]),
    }
}

const MAX_KEY_LEN: usize = 4 * 10 + 1;

impl DB {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<DB, rocksdb::Error> {
        let mut opts = rocksdb::Options::default();
        opts.create_if_missing(true);
        opts.set_compression_type(rocksdb::DBCompressionType::Snappy);
        opts.set_merge_operator("merge_proof", merge_proof, None);
        opts.set_memtable_prefix_bloom_ratio(0.1);
        opts.set_prefix_extractor(rocksdb::SliceTransform::create_fixed_prefix(5));

        opts.set_block_based_table_factory(&{
            let mut block_opts = rocksdb::BlockBasedOptions::default();
            block_opts.set_bloom_filter(10, false);
            block_opts.set_index_type(rocksdb::BlockBasedIndexType::HashSearch);
            block_opts
        });

        rocksdb::DB::open(&opts, path).map(|db| DB { rocks: db })
    }

    fn encode<'a>(buf: &'a mut [u8], g: &game::Game) -> &'a [u8] {
        let mut i = 0;
        buf[i..i + 4].copy_from_slice(&g.game_states.bits().to_le_bytes());
        i += 4;
        buf[i] = (g.next_board.map(|i| i + 1).unwrap_or(0)) << 4 | (g.next_player.as_bit() as u8);
        i += 1;
        let mut mask = !g.game_states.donebits();
        for b in 0..9 {
            if mask & 1 == 1 {
                let bits = (g.boards.xbits(b) << 16) | g.boards.obits(b);
                buf[i..i + 4].copy_from_slice(&bits.to_le_bytes());
                i += 4;
            }
            mask >>= 1;
        }
        &buf[0..i]
    }

    fn decode(buf: &[u8]) -> game::Game {
        let mut unpacked: game::Unpacked = Default::default();
        let ometa = u16::from_le_bytes(buf[0..2].try_into().unwrap());
        let xmeta = u16::from_le_bytes(buf[2..4].try_into().unwrap());
        for i in 0..9 {
            let st = match (xmeta & (1 << i) != 0, ometa & (1 << i) != 0) {
                (false, false) => game::BoardState::InPlay,
                (true, false) => game::BoardState::Won(game::Player::X),
                (false, true) => game::BoardState::Won(game::Player::O),
                (true, true) => game::BoardState::Drawn,
            };
            unpacked.game_states[i] = st;
        }
        unpacked.next_player = game::Player::from_bit(buf[4] & 1 == 1);
        unpacked.next_board = match buf[4] >> 4 {
            0 => None,
            n => Some(n - 1),
        };
        let mut i = 5;
        for board in 0..9 {
            if unpacked.game_states[board].terminal() {
                continue;
            }
            let o = u16::from_le_bytes(buf[i..i + 2].try_into().unwrap());
            let x = u16::from_le_bytes(buf[i + 2..i + 4].try_into().unwrap());
            i += 4;
            for cell in 0..9 {
                let st = match (x & (1 << cell) != 0, o & (1 << cell) != 0) {
                    (false, false) => game::CellState::Empty,
                    (true, false) => game::CellState::Played(game::Player::X),
                    (false, true) => game::CellState::Played(game::Player::O),
                    (true, true) => panic!("invalid cell: {},{}", board, cell),
                };
                unpacked.boards[board].0[cell] = st;
            }
        }
        let g = game::Game::pack(&unpacked);
        let mut alloc = [0; MAX_KEY_LEN];
        let reencode = DB::encode(&mut alloc, &g);
        assert!(
            reencode == buf,
            "encode->decode did not round trip got={:?} out={:?}",
            buf,
            reencode
        );
        g
    }

    pub fn lookup(&self, g: &game::Game) -> Option<ProofStatus> {
        let mut buf = [0; MAX_KEY_LEN];
        let key = DB::encode(&mut buf, g);
        self.rocks
            .get_pinned(key)
            .unwrap()
            .map(|bytes| ProofStatus::parse(&bytes).expect("expected valid bytes"))
    }

    pub fn write(&self, g: &game::Game, st: ProofStatus) {
        let mut buf = [0; MAX_KEY_LEN];
        let key = DB::encode(&mut buf, g);
        let val = [st.as_byte()];
        self.rocks.merge(key, &val).unwrap();
    }
}
