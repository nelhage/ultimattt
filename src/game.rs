mod display;
pub mod notation;

use std::vec::Vec;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Player {
    X,
    O,
}

impl Player {
    pub fn other(&self) -> Player {
        match self {
            Player::X => Player::O,
            Player::O => Player::X,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Player::X => "X",
            Player::O => "O",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CellState {
    Empty,
    Played(Player),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoardState {
    InPlay,
    Drawn,
    Won(Player),
}

impl BoardState {
    pub fn terminal(&self) -> bool {
        match self {
            BoardState::Drawn | BoardState::Won(_) => true,
            _ => false,
        }
    }
}

pub(in crate) const WIN_PATTERNS: [[usize; 3]; 8] = [
    // rows
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    // columns
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    // diagonals
    [0, 4, 8],
    [6, 4, 2],
];

pub(in crate) const WIN_MASKS: &[u32] = &[0x7, 0x38, 0x1c0, 0x49, 0x92, 0x124, 0x111, 0x54];
const BOARD_MASK: u32 = 0x1ff;

trait HasOwner {
    fn player(&self) -> Option<Player>;
    fn empty(&self) -> bool;
}

impl HasOwner for CellState {
    fn player(&self) -> Option<Player> {
        match self {
            CellState::Empty => None,
            CellState::Played(p) => Some(*p),
        }
    }

    fn empty(&self) -> bool {
        match self {
            CellState::Empty => true,
            _ => false,
        }
    }
}

impl HasOwner for BoardState {
    fn player(&self) -> Option<Player> {
        match self {
            BoardState::Won(p) => Some(*p),
            _ => None,
        }
    }
    fn empty(&self) -> bool {
        match self {
            BoardState::InPlay => true,
            _ => false,
        }
    }
}

fn check_winner<T: HasOwner>(board: &[T; 9], who: Player) -> BoardState {
    if WIN_PATTERNS.iter().any(|&pat| {
        pat.iter().all(|i| match board[*i].player() {
            Some(p) => p == who,
            _ => false,
        })
    }) {
        return BoardState::Won(who);
    }

    if board.iter().any(|e| e.empty()) {
        BoardState::InPlay
    } else {
        BoardState::Drawn
    }
}

#[derive(Clone, Debug)]
pub struct Subboard([CellState; 9]);

#[derive(Clone, Debug)]
pub struct Unpacked {
    pub next_player: Player,
    pub next_board: Option<u8>,
    pub boards: [Subboard; 9],
    pub game_states: [BoardState; 9],
    pub overall_state: BoardState,
}

impl Default for Unpacked {
    fn default() -> Self {
        let board = Subboard([CellState::Empty; 9]);
        Self {
            next_player: Player::X,
            next_board: None,
            boards: [
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
            ],
            game_states: [BoardState::InPlay; 9],
            overall_state: BoardState::InPlay,
        }
    }
}

#[derive(Clone, Debug)]
struct Row {
    // These are each a packed [u9; 3] containing a bitmask for the
    // respective player's states. The low bits store index 0.
    x: u32,
    o: u32,
}

#[derive(Clone, Debug)]
pub(in crate) struct Subboards {
    rows: [Row; 3],
}

impl Subboards {
    fn at(&self, board: usize, cell: usize) -> CellState {
        let row = &self.rows[board / 3];
        let idx = (board % 3) * 9 + cell;
        let xbit = (row.x >> idx) & 1 == 1;
        let obit = (row.o >> idx) & 1 == 1;
        if xbit {
            return CellState::Played(Player::X);
        }
        if obit {
            return CellState::Played(Player::O);
        }
        return CellState::Empty;
    }

    fn set(&mut self, board: usize, cell: usize, who: Player) {
        let mut row = &mut self.rows[board / 3];
        let idx = (board % 3) * 9 + cell;
        let bit = 1 << idx;
        match who {
            Player::X => {
                row.x |= bit;
            }
            Player::O => {
                row.o |= bit;
            }
        }
    }

    fn full(&self, board: usize) -> bool {
        let row = &self.rows[board / 3];
        let mask = BOARD_MASK << 9 * (board % 3);
        (row.o | row.x) & mask == mask
    }

    pub(in crate) fn xbits(&self, board: usize) -> u32 {
        let row = &self.rows[board / 3];
        (row.x >> (9 * (board % 3))) & BOARD_MASK
    }

    pub(in crate) fn obits(&self, board: usize) -> u32 {
        let row = &self.rows[board / 3];
        (row.o >> (9 * (board % 3))) & BOARD_MASK
    }

    fn mask(&self, board: usize) -> u32 {
        let row = &self.rows[board / 3];
        (row.o | row.x) >> (9 * (board % 3)) & BOARD_MASK
    }

    fn check_winner(&self, board: usize, player: Player) -> BoardState {
        let row = &self.rows[board / 3];
        let shift = 9 * (board % 3);
        let mask = match player {
            Player::X => row.x >> shift,
            Player::O => row.o >> shift,
        };
        for win in WIN_MASKS {
            if mask & win == *win {
                return BoardState::Won(player);
            }
        }
        if ((row.x | row.o) >> shift) & BOARD_MASK == BOARD_MASK {
            BoardState::Drawn
        } else {
            BoardState::InPlay
        }
    }
}

impl Default for Subboards {
    fn default() -> Self {
        Subboards {
            rows: [Row { x: 0, o: 0 }, Row { x: 0, o: 0 }, Row { x: 0, o: 0 }],
        }
    }
}

#[derive(Clone, Debug)]
pub(in crate) struct GameStates {
    // (x: u9, o: u9, drawn: u9), LSB first
    bits: u32,
}

impl Default for GameStates {
    fn default() -> Self {
        GameStates { bits: 0 }
    }
}

impl GameStates {
    pub(in crate) fn xbits(&self) -> u32 {
        self.bits & BOARD_MASK
    }
    pub(in crate) fn obits(&self) -> u32 {
        (self.bits >> 9) & BOARD_MASK
    }
    pub(in crate) fn drawbits(&self) -> u32 {
        (self.bits >> 18) & BOARD_MASK
    }
    pub(in crate) fn donebits(&self) -> u32 {
        (self.bits | (self.bits >> 9) | (self.bits >> 18)) & BOARD_MASK
    }
    pub(in crate) fn playerbits(&self, player: Player) -> u32 {
        match player {
            Player::X => self.xbits(),
            Player::O => self.obits(),
        }
    }
    pub(in crate) fn in_play(&self, board: usize) -> bool {
        (self.donebits() & 1 << board) == 0
    }

    fn check_winner(&self, player: Player) -> BoardState {
        let mask = self.playerbits(player);
        for win in WIN_MASKS {
            if mask & win == *win {
                return BoardState::Won(player);
            }
        }
        if self.donebits() == BOARD_MASK {
            BoardState::Drawn
        } else {
            BoardState::InPlay
        }
    }

    fn at(&self, board: usize) -> BoardState {
        let bit = 1 << board;
        if self.xbits() & bit != 0 {
            BoardState::Won(Player::X)
        } else if self.obits() & bit != 0 {
            BoardState::Won(Player::O)
        } else if self.drawbits() & bit != 0 {
            BoardState::Drawn
        } else {
            BoardState::InPlay
        }
    }

    fn set(&mut self, board: usize, state: BoardState) {
        let mut i = board;
        match state {
            BoardState::Drawn => {
                i += 18;
            }
            BoardState::InPlay => {
                return;
            }
            BoardState::Won(Player::X) => {}
            BoardState::Won(Player::O) => {
                i += 9;
            }
        };
        self.bits |= 1 << i;
    }
}

#[derive(Clone, Debug)]
pub struct Game {
    pub(in crate) next_player: Player,
    pub(in crate) next_board: Option<u8>,
    pub(in crate) boards: Subboards,
    pub(in crate) game_states: GameStates,
    pub(in crate) overall_state: BoardState,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Move {
    bits: u8,
}

impl Move {
    pub fn from_coords(board: usize, square: usize) -> Self {
        return Self {
            bits: (board << 4 | square) as u8,
        };
    }

    pub fn board(self) -> usize {
        (self.bits >> 4) as usize
    }

    pub fn square(self) -> usize {
        (self.bits & 0xf) as usize
    }

    pub fn none() -> Self {
        Move { bits: 0xff }
    }

    pub fn is_none(self) -> bool {
        self.bits == 0xff
    }

    pub fn is_some(self) -> bool {
        !self.is_none()
    }

    pub fn bits(self) -> u8 {
        self.bits
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum MoveError {
    WrongBoard,
    WonBoard,
    OutOfBounds,
    NotEmpty,
    GameOver,
}

impl Game {
    pub fn new() -> Game {
        Game {
            next_player: Player::X,
            next_board: None,
            boards: Default::default(),
            game_states: Default::default(),
            overall_state: BoardState::InPlay,
        }
    }

    pub fn pack(bits: &Unpacked) -> Game {
        let mut boards: Subboards = Default::default();
        for row in 0..3 {
            let mut x: u32 = 0;
            let mut o: u32 = 0;
            for col in 0..3 {
                let board = &bits.boards[3 * row + col];
                for (i, cell) in board.0.iter().enumerate() {
                    let idx: u32 = 1 << (9 * col + i);
                    match cell {
                        CellState::Played(Player::X) => {
                            x |= idx;
                        }
                        CellState::Played(Player::O) => {
                            o |= idx;
                        }
                        CellState::Empty => (),
                    }
                }
            }
            boards.rows[row].x = x;
            boards.rows[row].o = o;
        }
        let mut states: GameStates = Default::default();
        for i in 0..9 {
            states.set(i, bits.game_states[i]);
        }
        Game {
            next_player: bits.next_player,
            next_board: bits.next_board,
            boards: boards,
            game_states: states,
            overall_state: bits.overall_state,
        }
    }

    pub fn make_move(&self, m: Move) -> Result<Game, MoveError> {
        let mut out = self.clone();
        out.inplace_move(m).map(|_| out)
    }

    pub fn inplace_move(&mut self, m: Move) -> Result<(), MoveError> {
        if m.board() >= 9 || m.square() >= 9 {
            return Err(MoveError::OutOfBounds);
        }
        match self.overall_state {
            BoardState::InPlay => (),
            _ => return Err(MoveError::GameOver),
        }
        if !self.game_states.in_play(m.board()) {
            return Err(MoveError::WonBoard);
        }
        if let Some(b) = self.next_board {
            if b != m.board() as u8 {
                return Err(MoveError::WrongBoard);
            }
        }
        if self.boards.at(m.board(), m.square()) != CellState::Empty {
            return Err(MoveError::NotEmpty);
        }

        self.boards.set(m.board(), m.square(), self.next_player);
        self.game_states.set(
            m.board(),
            self.boards.check_winner(m.board(), self.next_player),
        );
        if self.game_states.in_play(m.square()) {
            self.next_board = Some(m.square() as u8);
        } else {
            self.next_board = None;
        }
        self.overall_state = self.game_states.check_winner(self.next_player);
        self.next_player = self.next_player.other();
        return Ok(());
    }

    pub fn recalc_winner(&mut self) {
        self.overall_state = self.game_states.check_winner(self.next_player.other());
    }

    pub fn all_moves<'a>(&'a self) -> impl Iterator<Item = Move> + 'a {
        MoveIterator::from_game(&self)
    }

    pub fn game_state(&self) -> BoardState {
        self.overall_state
    }

    pub fn player(&self) -> Player {
        self.next_player
    }

    pub fn board_to_play(&self) -> Option<usize> {
        self.next_board.map(|b| b as usize)
    }

    pub fn at(&self, board: usize, cell: usize) -> CellState {
        self.boards.at(board, cell)
    }

    pub fn board_state(&self, board: usize) -> BoardState {
        self.game_states.at(board)
    }
}

struct MoveIterator<'a> {
    game: &'a Game,
    board: usize,
    mask: u32,
    cell: usize,
}

impl<'a> MoveIterator<'a> {
    fn from_game(game: &'a Game) -> Self {
        let board = match game.board_to_play() {
            Some(b) => b,
            None => 0,
        };
        MoveIterator {
            game: game,
            cell: 0,
            mask: game.boards.mask(board),
            board: board,
        }
    }
}

impl<'a> Iterator for MoveIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cell >= 9 {
                if let Some(_) = self.game.board_to_play() {
                    return None;
                }
                self.board += 1;
                if self.board >= 9 {
                    return None;
                }
                self.cell = 0;
                self.mask = self.game.boards.mask(self.board);
            }

            let bit = self.mask & 1 == 0;
            let cell = self.cell;
            self.mask >>= 1;
            self.cell += 1;
            if bit {
                return Some(Move::from_coords(self.board, cell));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn board(moves: &[(usize, usize)]) -> Game {
        let mut g = Game::new();
        for (i, m) in moves.iter().enumerate() {
            match g.make_move(Move::from_coords(m.0, m.1)) {
                Ok(g_) => g = g_,
                Err(e) => panic!("move failed: {:?}: {:?}\n{}", m, e, &g),
            }
            let at = g.at(m.0, m.1);
            if at != CellState::Played(g.player().other()) {
                panic!(
                    "board(): consistency failure i={} m={:?} bits={:?} at={:?}",
                    i, m, g.boards, at
                );
            }
        }
        g
    }

    #[test]
    fn test_move() {
        let g = Game::new();
        match g.make_move(Move::from_coords(1, 6)) {
            Ok(gg) => {
                assert_eq!(gg.at(1, 6), CellState::Played(Player::X));
                assert_eq!(gg.at(0, 0), CellState::Empty);
                for i in 0..9 {
                    assert_eq!(gg.board_state(i), BoardState::InPlay);
                }
            }
            Err(e) => {
                panic!("move failed: {:?}", e);
            }
        };
    }

    #[test]
    fn test_sub_win() {
        let g = board(&[(0, 4), (4, 0), (0, 2), (2, 0), (0, 6)]);
        assert_eq!(g.board_state(0), BoardState::Won(Player::X));

        let g = board(&[(0, 4), (4, 6), (6, 4), (4, 7), (7, 4), (4, 8)]);
        assert_eq!(g.board_state(4), BoardState::Won(Player::O));
    }

    #[test]
    fn test_sub_draw() {
        let g = board(&[
            (0, 2),
            (2, 0),
            (0, 4),
            (4, 0),
            (0, 7),
            (7, 0),
            (0, 0),
            (0, 1),
            (1, 0),
            (0, 3),
            (3, 0),
            (0, 5),
            (5, 0),
            (0, 6),
            (6, 0),
            (0, 8),
        ]);
        assert_eq!(g.board_state(0), BoardState::Drawn);
    }

    #[test]
    fn test_move_anywhere() {
        let g = board(&[
            (0, 2),
            (2, 0),
            (0, 4),
            (4, 0),
            (0, 7),
            (7, 0),
            (0, 0),
            (0, 1),
            (1, 0),
            (0, 3),
            (3, 0),
            (0, 5),
            (5, 0),
            (0, 6),
            (6, 0),
            (0, 8),
            (8, 0),
        ]);
        for b in &[1, 2, 3, 4, 5, 6, 7, 8] {
            let r = g.make_move(Move::from_coords(*b, *b));
            if let Err(e) = r {
                panic!("Disallowed move: ({}, {}): {:?}", b, b, e);
            }
        }
        assert_eq!(g.board_state(0), BoardState::Drawn);
    }

    #[test]
    fn test_move_anywhere_won() {
        let g = board(&[(0, 4), (4, 0), (0, 2), (2, 0), (0, 6), (6, 0)]);
        for b in &[1, 2, 3, 4, 5, 6, 7, 8] {
            let r = g.make_move(Move::from_coords(*b, *b));
            if let Err(e) = r {
                panic!("Disallowed move: ({}, {}): {:?}", b, b, e);
            }
        }
        let r = g.make_move(Move::from_coords(0, 1));
        if let Ok(_) = r {
            panic!("Can't move into a won board");
        }
    }

    #[test]
    fn test_win() {
        let g = board(&[
            (0, 2),
            (2, 0),
            (0, 4),
            (4, 0),
            (0, 6),
            (6, 1),
            (1, 2),
            (2, 1),
            (1, 4),
            (4, 1),
            (1, 6),
            (6, 2),
            (2, 3),
            (3, 2),
            (2, 4),
            (4, 2),
            (2, 5),
        ]);
        assert_eq!(g.game_state(), BoardState::Won(Player::X));
        let r = g.make_move(Move::from_coords(5, 1));
        match r {
            Ok(_) => panic!("allowed move on won board"),
            Err(e) => {
                assert_eq!(e, MoveError::GameOver);
            }
        }
    }

    #[test]
    fn test_move_errors() {
        let g = board(&[(0, 4), (4, 0)]);
        let cases = &[
            (1, 4, MoveError::WrongBoard),
            (0, 4, MoveError::NotEmpty),
            (0, 10, MoveError::OutOfBounds),
            (9, 3, MoveError::OutOfBounds),
        ];
        for (board, square, expect) in cases {
            let m = g.make_move(Move::from_coords(*board, *square));
            match m {
                Ok(_) => panic!("Move succeeded, expected {:?}", expect),
                Err(e) => {
                    if e != *expect {
                        panic!(
                            "Move returned wrong error: got {:?}, expected {:?}",
                            e, expect
                        )
                    }
                }
            }
        }
    }

    #[test]
    fn test_all_moves() {
        let cases = &[
            (Game::new(), 81),
            (board(&[(0, 4)]), 9),
            (board(&[(4, 4)]), 8),
        ];
        for (board, count) in cases {
            assert_eq!(*count, board.all_moves().collect::<Vec<Move>>().len());
            for m in board.all_moves() {
                if let Err(e) = board.make_move(m) {
                    panic!("Bad move: {:?}: {:?})", m, e);
                }
            }
        }
    }
}
