#[allow(dead_code)]
mod game;

extern crate ansi_term;
use ansi_term::{ANSIString, Color, Style};

use std::io;

fn cell(g: game::CellState) -> &'static str {
    match g {
        game::CellState::Empty => ".",
        game::CellState::Played(game::Player::X) => "X",
        game::CellState::Played(game::Player::O) => "O",
    }
}

fn stylefor(g: &game::Game, boards: &[usize]) -> Style {
    let style = Style::default();
    if boards
        .iter()
        .any(|b| g.board_to_play().map(|n| n == *b).unwrap_or(false))
    {
        return style.underline();
    }
    return style;
}

fn render(out: &mut dyn io::Write, g: &game::Game) -> Result<(), io::Error> {
    for brow in 0..3 {
        if brow != 0 {
            write!(out, "---+---+---\n")?;
        }
        for bcol in 0..3 {
            if bcol != 0 {
                write!(out, " |")?;
            }
            let ch = match g.board_state(3 * brow + bcol as usize) {
                game::BoardState::InPlay => " ",
                game::BoardState::Drawn => "#",
                game::BoardState::Won(game::Player::X) => "X",
                game::BoardState::Won(game::Player::O) => "O",
            };
            write!(out, " {}", ch)?;
        }
        write!(out, "\n")?;
    }
    writeln!(out, "")?;

    for row in 0..9 {
        if row > 0 && row % 3 == 0 {
            writeln!(out, "--------+---------+--------")?;
        }
        for col in 0..9 {
            let board = 3 * (row / 3) + col / 3;
            let sq = 3 * (row % 3) + col % 3;
            let at = g.at(board, sq);
            let ch = match at {
                game::CellState::Empty => {
                    if g.board_to_play().map(|b| b == board).unwrap_or(false) {
                        &"abcdefghi"[sq..sq + 1]
                    } else {
                        "."
                    }
                }
                other => cell(other),
            };
            write!(out, "{}", stylefor(g, &[board]).paint(ch))?;
            if col != 8 {
                match col % 3 {
                    0 | 1 => {
                        write!(out, "{}", stylefor(g, &[board]).paint("  "))?;
                    }
                    2 => {
                        write!(out, "{}", stylefor(g, &[board, board + 1]).paint(" "))?;
                        write!(out, "{}", stylefor(g, &[board + 1]).paint("| "))?;
                    }
                    _ => unreachable!(),
                };
            }
        }
        write!(out, "\n")?;
    }
    Ok(())
}

fn read_move(g: &game::Game) -> Result<game::Move, io::Error> {
    render(&mut io::stdout(), g)?;
    Err(io::Error::new(io::ErrorKind::Other, "unimplemented"))
}

fn main() -> Result<(), std::io::Error> {
    let mut g = game::Game::new();
    g = g
        .make_move(game::Move {
            board: 3,
            square: 0,
        })
        .unwrap();
    loop {
        let m = read_move(&g)?;

        match g.make_move(m) {
            Ok(gg) => {
                g = gg;
            }
            Err(e) => {
                println!("bad move: {:?}", e);
            }
        };
        match g.game_state() {
            game::BoardState::InPlay => (),
            _ => break,
        }
    }
    Ok(())
}
