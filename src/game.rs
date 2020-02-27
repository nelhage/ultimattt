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

    pub fn bit(&self) -> usize {
        match self {
            Player::X => 1,
            Player::O => 0,
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

const WIN_PATTERNS: [[usize; 3]; 8] = [
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
    pub next_board: Option<usize>,
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
struct ToPlay(u8);

impl ToPlay {
    fn pack(player: Player, board: Option<usize>) -> Self {
        Self((player.bit() as u8) << 7 | board.unwrap_or(0xf) as u8)
    }

    fn player(&self) -> Player {
        if (self.0 >> 7) == 1 {
            Player::X
        } else {
            Player::O
        }
    }

    fn board(&self) -> Option<usize> {
        if self.0 & 0xf == 0xf {
            None
        } else {
            Some((self.0 & 0xf) as usize)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Game {
    to_play: ToPlay,
    boards: [Subboard; 9],
    game_states: [BoardState; 9],
    overall_state: BoardState,
}

#[derive(Copy, Clone, Debug)]
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum MoveError {
    WrongBoard,
    OutOfBounds,
    NotEmpty,
    GameOver,
}

impl Game {
    pub fn new() -> Game {
        let board = Subboard([CellState::Empty; 9]);
        Game {
            to_play: ToPlay::pack(Player::X, None),
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

    pub fn pack(bits: &Unpacked) -> Game {
        Game {
            to_play: ToPlay::pack(bits.next_player, bits.next_board),
            boards: bits.boards.clone(),
            game_states: bits.game_states.clone(),
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
        if let Some(b) = self.board_to_play() {
            if b != m.board() {
                return Err(MoveError::WrongBoard);
            }
        }
        if self.boards[m.board()].0[m.square()] != CellState::Empty {
            return Err(MoveError::NotEmpty);
        }

        self.boards[m.board()].0[m.square()] = CellState::Played(self.player());
        if let BoardState::InPlay = self.game_states[m.board()] {
            self.game_states[m.board()] = check_winner(&self.boards[m.board()].0, self.player());
        }
        let next_board = if self.boards[m.square()]
            .0
            .iter()
            .any(|s| *s == CellState::Empty)
        {
            Some(m.square())
        } else {
            None
        };
        self.overall_state = check_winner(&self.game_states, self.player());
        self.to_play = ToPlay::pack(self.player().other(), next_board);
        return Ok(());
    }

    fn recalc_winner(&mut self) {
        self.overall_state = check_winner(&self.game_states, self.player().other());
    }

    fn moves_on(&self, board: usize, out: &mut Vec<Move>) {
        for sq in 0..9 {
            if let CellState::Empty = self.boards[board].0[sq] {
                out.push(Move::from_coords(board, sq))
            }
        }
    }

    pub fn all_moves(&self) -> Vec<Move> {
        let mut out = Vec::new();
        match self.board_to_play() {
            Some(b) => self.moves_on(b as usize, &mut out),
            None => {
                for b in 0..9 {
                    self.moves_on(b, &mut out);
                }
            }
        }
        out
    }

    pub fn game_state(&self) -> BoardState {
        self.overall_state
    }

    pub fn player(&self) -> Player {
        self.to_play.player()
    }

    pub fn board_to_play(&self) -> Option<usize> {
        self.to_play.board()
    }

    pub fn at(&self, board: usize, cell: usize) -> CellState {
        self.boards[board].0[cell]
    }

    pub fn board_state(&self, board: usize) -> BoardState {
        self.game_states[board]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn board(moves: &[(usize, usize)]) -> Game {
        let mut g = Game::new();
        for m in moves {
            match g.make_move(Move::from_coords(m.0, m.1)) {
                Ok(g_) => g = g_,
                Err(e) => panic!("move failed: {:?}: {:?}\n{}", m, e, &g),
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
            assert_eq!(*count, board.all_moves().len());
            for m in board.all_moves() {
                if let Err(e) = board.make_move(m) {
                    panic!("Bad move: {:?}: {:?})", m, e);
                }
            }
        }
    }
}
