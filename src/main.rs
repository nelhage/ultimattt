#![feature(test)]
#[allow(dead_code)]
mod game;
mod minimax;

extern crate ansi_term;
extern crate rand;
extern crate structopt;

use crate::minimax::AI;
use ansi_term::Style;
use std::io;
use std::io::Write;
use std::process::exit;
use std::time::Duration;
use structopt::StructOpt;

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

struct CLIPlayer<'a> {
    stdin: Box<dyn io::BufRead + 'a>,
    stdout: Box<dyn io::Write + 'a>,
}

impl<'a> minimax::AI for CLIPlayer<'a> {
    fn select_move(&mut self, g: &game::Game) -> game::Move {
        render(&mut self.stdout, g).unwrap();
        loop {
            write!(&mut self.stdout, "move> ").unwrap();
            self.stdout.flush().unwrap();

            let mut line = String::new();
            if let Ok(b) = self.stdin.read_line(&mut line) {
                if b == 0 {
                    exit(0);
                }
            } else {
                exit(0);
            }
            match game::notation::parse_move(line.trim_end()) {
                Ok(m) => {
                    return m;
                }
                Err(e) => {
                    writeln!(&mut self.stdout, "Bad move: {:?}", e).unwrap();
                }
            }
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "ultimatett", about = "Ultimate Tic Tac Toe")]
struct Opt {
    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(Debug, StructOpt)]
enum Command {
    Play {},
    Analyze { position: String },
}

fn main() -> Result<(), std::io::Error> {
    let opt = Opt::from_args();
    match opt.cmd {
        Command::Play { .. } => {
            let mut g = game::Game::new();
            let stdin = io::stdin();
            let mut player_x: Box<dyn minimax::AI> = Box::new(CLIPlayer {
                stdin: Box::new(stdin.lock()),
                stdout: Box::new(io::stdout()),
            });
            let mut player_o: Box<dyn minimax::AI> =
                Box::new(minimax::Minimax::with_timeout(Duration::from_secs(1)));

            loop {
                let m = match g.player() {
                    game::Player::X => player_x.select_move(&g),
                    game::Player::O => player_o.select_move(&g),
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
        }
        Command::Analyze { position } => {
            let game = match game::notation::parse(&position) {
                Ok(g) => g,
                Err(e) => {
                    println!("Parsing position: {:?}", e);
                    exit(1)
                }
            };
            let mut ai = minimax::Minimax::with_timeout(Duration::from_secs(1));
            let m = ai.select_move(&game);
            println!("move={}", game::notation::render_move(m));
        }
    }
    Ok(())
}
