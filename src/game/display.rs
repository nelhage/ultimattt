use std::fmt;

use super::{BoardState, CellState, Game, Player};

mod pr {
    use super::{BoardState, CellState, Game, Player};
    use std::fmt;

    pub fn player(p: Player) -> &'static str {
        match p {
            Player::X => "X",
            Player::O => "O",
        }
    }

    pub fn board_state(p: BoardState) -> &'static str {
        match p {
            BoardState::InPlay => " ",
            BoardState::Drawn => "#",
            BoardState::Won(p) => player(p),
        }
    }

    pub fn cell_state(b: CellState) -> &'static str {
        match b {
            CellState::Empty => ".",
            CellState::Played(p) => player(p),
        }
    }

    pub fn overall_row(f: &mut fmt::Formatter, g: &Game, row: usize) -> fmt::Result {
        write!(
            f,
            "{} | {} | {}\n",
            board_state(g.board_state(3 * row)),
            board_state(g.board_state(3 * row + 1)),
            board_state(g.board_state(3 * row + 2))
        )
    }

    pub fn board_row(f: &mut fmt::Formatter, g: &Game, board: usize, row: usize) -> fmt::Result {
        write!(
            f,
            "{}  {}  {}",
            cell_state(g.at(board, 3 * row)),
            cell_state(g.at(board, 3 * row + 1)),
            cell_state(g.at(board, 3 * row + 2)),
        )
    }

    pub fn row(f: &mut fmt::Formatter, g: &Game, row: usize) -> fmt::Result {
        let board_base = 3 * (row / 3);
        let subrow = row % 3;

        board_row(f, g, board_base, subrow)?;
        write!(f, " | ")?;
        board_row(f, g, board_base + 1, subrow)?;
        write!(f, " | ")?;
        board_row(f, g, board_base + 2, subrow)?;
        write!(f, "\n")
    }
}

impl fmt::Display for super::Game {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        pr::overall_row(f, self, 0)?;
        write!(f, "--+---+--\n")?;
        pr::overall_row(f, self, 1)?;
        write!(f, "--+---+--\n")?;
        pr::overall_row(f, self, 2)?;
        write!(f, "\n")?;

        for row in 0..9 {
            if row > 0 && row % 3 == 0 {
                write!(f, "--------+---------+--------\n")?;
            }
            pr::row(f, self, row)?;
        }

        Ok(())
    }
}
