mod display;

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

#[derive(Clone, Debug)]
pub struct Subboard([CellState; 9]);

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
pub struct Game {
    next_player: Player,
    next_board: Option<u8>,
    boards: [Subboard; 9],
    game_states: [BoardState; 9],
    overall_state: BoardState,
}

#[derive(Copy, Clone, Debug)]
pub struct Move {
    pub board: u8,
    pub square: u8,
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

    pub fn make_move(&self, m: Move) -> Result<Game, MoveError> {
        let mut out = self.clone();
        out.inplace_move(m).map(|_| out)
    }

    pub fn inplace_move(&mut self, m: Move) -> Result<(), MoveError> {
        if m.board >= 9 || m.square >= 9 {
            return Err(MoveError::OutOfBounds);
        }
        match self.overall_state {
            BoardState::InPlay => (),
            _ => return Err(MoveError::GameOver),
        }
        if let Some(b) = self.next_board {
            if b != m.board {
                return Err(MoveError::WrongBoard);
            }
        }
        if self.boards[m.board as usize].0[m.square as usize] != CellState::Empty {
            return Err(MoveError::NotEmpty);
        }

        self.boards[m.board as usize].0[m.square as usize] = CellState::Played(self.next_player);
        if let BoardState::InPlay = self.game_states[m.board as usize] {
            self.game_states[m.board as usize] =
                check_winner(&self.boards[m.board as usize].0, self.next_player);
        }
        if self.boards[m.square as usize]
            .0
            .iter()
            .any(|s| *s == CellState::Empty)
        {
            self.next_board = Some(m.square);
        } else {
            self.next_board = None;
        }
        self.overall_state = check_winner(&self.game_states, self.next_player);
        self.next_player = self.next_player.other();
        return Ok(());
    }

    fn moves_on(&self, board: u8, out: &mut Vec<Move>) {
        for sq in 0..9 {
            if let CellState::Empty = self.boards[board as usize].0[sq] {
                out.push(Move {
                    board: board,
                    square: sq as u8,
                })
            }
        }
    }

    pub fn all_moves(&self) -> Vec<Move> {
        let mut out = Vec::new();
        match self.next_board {
            Some(b) => self.moves_on(b, &mut out),
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
        self.next_player
    }

    pub fn board_to_play(&self) -> Option<usize> {
        self.next_board.map(|b| b as usize)
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
            match g.make_move(Move {
                board: m.0 as u8,
                square: m.1 as u8,
            }) {
                Ok(g_) => g = g_,
                Err(e) => panic!("move failed: {:?}: {:?}\n{}", m, e, &g),
            }
        }
        g
    }

    #[test]
    fn test_move() {
        let g = Game::new();
        match g.make_move(Move {
            board: 1,
            square: 6,
        }) {
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
            let r = g.make_move(Move {
                board: *b,
                square: *b,
            });
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
        let r = g.make_move(Move {
            board: 5,
            square: 1,
        });
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
            let m = g.make_move(Move {
                board: *board as u8,
                square: *square as u8,
            });
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
