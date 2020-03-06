#![feature(test)]
#[allow(dead_code)]
mod game;
mod minimax;

extern crate ansi_term;
use ansi_term::Style;
use std::time::Duration;

extern crate rand;

use std::io;
use std::io::Write;

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
        write!(out, "      ")?;
        let ch = 'a' as u8 + 3 * (brow as u8);
        write!(
            out,
            "{}    {}    {}",
            ch as char,
            (ch + 1) as char,
            (ch + 2) as char,
        )?;
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
            write!(out, "{}", stylefor(g, &[board]).paint(cell(at)))?;
            if col != 8 {
                match col % 3 {
                    0 | 1 => {
                        write!(out, "{}", stylefor(g, &[board]).paint("  "))?;
                    }
                    2 => {
                        write!(out, "{}", stylefor(g, &[board]).paint(" "))?;
                        write!(out, "|")?;
                        write!(out, "{}", stylefor(g, &[board + 1]).paint(" "))?;
                    }
                    _ => unreachable!(),
                };
            }
        }
        write!(out, "\n")?;
    }

    write!(out, "notation:\n{}\n", game::notation::render(g))?;

    Ok(())
}

fn read_move(g: &game::Game, ai: &mut dyn minimax::AI) -> Result<game::Move, io::Error> {
    match g.player() {
        game::Player::X => {
            let mut out = io::stdout();
            render(&mut out, g)?;
            write!(&mut out, "move> ")?;
            out.flush()?;

            let mut line = String::new();
            if io::stdin().read_line(&mut line)? == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "EOF while reading move",
                ));
            }
            game::notation::parse_move(&line)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))
        }
        game::Player::O => Ok(ai.select_move(g)),
    }
}

fn main() -> Result<(), std::io::Error> {
    let mut g = game::Game::new();
    let mut ai = minimax::Minimax::with_timeout(Duration::from_secs(1));

    loop {
        let m = match read_move(&g, &mut ai) {
            Ok(m) => m,
            Err(e) => {
                if let io::ErrorKind::UnexpectedEof = e.kind() {
                    return Ok(());
                }
                println!("reading move: {:?}", e);
                continue;
            }
        };

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
