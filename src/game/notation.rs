use super::*;

pub fn render(g: &Game) -> String {
    let mut out = String::new();
    out.push_str(&g.player().to_string());
    out.push_str(";");
    for board in 0..9 {
        let st = g.board_state(board as usize);
        let ch = match st {
            BoardState::InPlay => ".",
            BoardState::Drawn => "#",
            BoardState::Won(p) => p.as_str(),
        };
        if g.board_to_play().map(|b| b == board).unwrap_or(false) {
            out.push_str("@");
        } else {
            out.push_str(ch);
        }
    }
    out.push_str(";");
    for board in 0..9 {
        for sq in 0..9 {
            let ch = match g.at(board as usize, sq as usize) {
                CellState::Empty => ".",
                CellState::Played(p) => p.as_str(),
            };
            out.push_str(ch);
        }
        if board != 8 {
            out.push_str("/");
        }
    }

    out
}

pub fn render_move(m: Move) -> String {
    let board = 'a' as u8 + m.board() as u8;
    let cell = 'a' as u8 + m.square() as u8;
    return format!("{}{}", board as char, cell as char);
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    NotImplemented,
    MissingSection,
    TooFewSquares,
    BadPlayer,
    BadBoard,
    TooShort,
}

#[derive(Clone, Debug)]
pub struct ParseError<'a> {
    loc: &'a str,
    why: ErrorKind,
}

pub fn parse(text: &str) -> Result<Game, ParseError> {
    let bits: Vec<&str> = text.split(";").collect();
    if bits.len() != 3 {
        return Err(ParseError {
            loc: text,
            why: ErrorKind::MissingSection,
        });
    }

    let mut game: Unpacked = Default::default();
    let who = bits[0];
    game.next_player = match who {
        "X" => Player::X,
        "O" => Player::O,
        _ => {
            return Err(ParseError {
                loc: who,
                why: ErrorKind::BadPlayer,
            })
        }
    };

    let overall = bits[1];
    if overall.len() != 9 {
        return Err(ParseError {
            loc: bits[1],
            why: ErrorKind::TooFewSquares,
        });
    }
    for board in 0..9 {
        let state = match overall.chars().nth(board).unwrap() {
            'X' => BoardState::Won(Player::X),
            'O' => BoardState::Won(Player::O),
            '#' => BoardState::Drawn,
            '@' => {
                game.next_board = Some(board as u8);
                BoardState::InPlay
            }
            '.' => BoardState::InPlay,
            _ => {
                return Err(ParseError {
                    loc: &overall[board..board + 1],
                    why: ErrorKind::BadBoard,
                });
            }
        };
        game.game_states[board] = state;
    }
    let mut chars = bits[2].chars();
    for board in 0..9 {
        for sq in 0..9 {
            if let Some(ch) = chars.next() {
                let state = match ch {
                    '.' => CellState::Empty,
                    'X' => CellState::Played(Player::X),
                    'O' => CellState::Played(Player::O),
                    _ => {
                        return Err(ParseError {
                            loc: bits[2],
                            why: ErrorKind::BadPlayer,
                        });
                    }
                };
                game.boards[board].0[sq] = state;
            } else {
                return Err(ParseError {
                    loc: &bits[2],
                    why: ErrorKind::TooShort,
                });
            }
        }
        if board != 8 {
            if let Some('/') = chars.next() {
            } else {
                return Err(ParseError {
                    loc: &bits[2],
                    why: ErrorKind::BadBoard,
                });
            }
        }
    }

    let mut out = Game::pack(&game);
    out.recalc_winner();

    return Ok(out);
}

#[derive(Clone, Debug)]
pub enum ParseMoveError {
    BadLength,
    BadCharacter(char),
}

fn parse_coord(ch: char) -> Result<usize, ParseMoveError> {
    if ch < 'a' || ch > 'i' {
        return Err(ParseMoveError::BadCharacter(ch));
    }
    Ok((ch as u8 - 'a' as u8) as usize)
}

pub fn parse_move(text: &str) -> Result<Move, ParseMoveError> {
    if text.len() != 2 {
        return Err(ParseMoveError::BadLength);
    }
    let mut chars = text.chars();
    let board = parse_coord(chars.next().unwrap())?;
    let square = parse_coord(chars.next().unwrap())?;
    Ok(Move::from_coords(board, square))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let g = Game::new()
            .make_move(Move::from_coords(1, 6))
            .unwrap()
            .make_move(Move::from_coords(6, 3))
            .unwrap();
        let notation = render(&g);
        let got = parse(&notation).unwrap();
        assert!(g.board_to_play() == got.board_to_play());
        for b in 0..9 {
            assert!(g.board_state(b as usize) == got.board_state(b as usize));
            for sq in 0..9 {
                assert!(g.at(b as usize, sq as usize) == got.at(b as usize, sq as usize));
            }
        }
    }
}
