mod display;

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

#[derive(Debug)]
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
        match self.overall_state {
            BoardState::InPlay => (),
            _ => return Err(MoveError::GameOver),
        }
        if let Some(b) = self.next_board {
            if b != m.board {
                return Err(MoveError::WrongBoard);
            }
        }
        if m.board > 9 || m.square > 9 {
            return Err(MoveError::OutOfBounds);
        }
        if self.boards[m.board as usize].0[m.square as usize] != CellState::Empty {
            return Err(MoveError::NotEmpty);
        }
        let mut out = self.clone();
        out.boards[m.board as usize].0[m.square as usize] = CellState::Played(out.next_player);
        if let BoardState::InPlay = out.game_states[m.board as usize] {
            out.game_states[m.board as usize] =
                check_winner(&out.boards[m.board as usize].0, out.next_player);
        }
        if out.boards[m.square as usize]
            .0
            .iter()
            .any(|s| *s == CellState::Empty)
        {
            out.next_board = Some(m.board);
        } else {
            out.next_board = None;
        }
        out.overall_state = check_winner(&out.game_states, out.next_player);
        out.next_player = out.next_player.other();
        return Ok(out);
    }

    pub fn game_state(&self) -> BoardState {
        self.overall_state
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
}
