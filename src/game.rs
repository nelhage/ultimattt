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

impl Subboard {
    fn compute_win(&self, who: Player) -> BoardState {
        if WIN_PATTERNS.iter().any(|&pat| {
            pat.iter().all(|i| match self.0[*i] {
                CellState::Played(p) => p == who,
                _ => false,
            })
        }) {
            return BoardState::Won(who);
        }

        if self.0.iter().any(|e| match e {
            CellState::Empty => true,
            _ => false,
        }) {
            BoardState::InPlay
        } else {
            BoardState::Drawn
        }
    }
}

#[derive(Clone, Debug)]
pub struct Game {
    next_player: Player,
    next_board: Option<u8>,
    boards: [Subboard; 9],
    game_states: [BoardState; 9],
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
        }
    }

    pub fn make_move(&self, m: Move) -> Result<Game, MoveError> {
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
                out.boards[m.board as usize].compute_win(out.next_player);
        }
        out.next_player = out.next_player.other();
        return Ok(out);
    }
}
