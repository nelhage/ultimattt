#![allow(dead_code)]

use crate::game;
use crate::prove;

use packed_simd::u16x8;

pub struct Analysis<'a> {
    pos: &'a game::Game,
    proven: prove::Status,
    attacker_critical: u16,
    defender_critical: u16,
}

impl<'a> Analysis<'a> {
    pub fn new(pos: &'a game::Game) -> Self {
        Self::analyze(pos)
    }

    fn analyze(pos: &'a game::Game) -> Self {
        if pos.game_over() {
            return Analysis {
                pos,
                attacker_critical: 0,
                defender_critical: 0,
                proven: prove::Status::unproven(),
            };
        }

        let attacker_critical = critical_boards(pos, pos.player());
        let defender_critical = critical_boards(pos, pos.player().other());
        let mut proven = prove::Status::unproven();

        if prove_attacker(pos, attacker_critical, defender_critical) {
            proven = prove::Status::for_player(pos.player())
        } else if forced_loss(pos, defender_critical) {
            proven = prove::Status::for_player(pos.player().other());
        }

        if !is_winnable(pos, game::Player::X) {
            proven = proven.merge(prove::Status::draw_or_o()).unwrap_or_else(|| {
                panic!(
                    "endgame merge({}): proven={:x?} but X can't win",
                    game::notation::render(pos),
                    proven
                );
            });
        }
        if !is_winnable(pos, game::Player::O) {
            proven = proven.merge(prove::Status::draw_or_x()).unwrap_or_else(|| {
                panic!(
                    "endgame merge({}): proven={:x?} but O can't win",
                    game::notation::render(pos),
                    proven
                );
            });
        }

        Analysis {
            pos,
            attacker_critical,
            defender_critical,
            proven,
        }
    }

    pub fn status(&self) -> prove::Status {
        return self.proven;
    }

    pub fn evaluate_move(&self, m: game::Move) -> prove::Status {
        if self.defender_critical == 0 {
            return prove::Status::unproven();
        }
        let bad = self.pos.game_states.donebits() | (self.defender_critical as u32);
        if bad & (1 << m.board()) != 0 {
            return prove::Status::for_player(self.pos.player().other());
        }
        return prove::Status::unproven();
    }
}

fn is_winnable(pos: &game::Game, by: game::Player) -> bool {
    let potential = pos.game_states.playerbits(by) | !pos.game_states.donebits();
    return (u16x8::splat(potential as u16) & game::WIN_MASKS_SIMD)
        .eq(game::WIN_MASKS_SIMD)
        .any();
}

fn prove_attacker(pos: &game::Game, attacker_critical: u16, defender_critical: u16) -> bool {
    if attacker_critical == 0 {
        return false;
    }

    // If the attacker has a critial square, and is permitted
    // to play there, she wins.
    let board = match pos.board_to_play() {
        None => return true,
        Some(b) if attacker_critical & (1 << b) != 0 => return true,
        Some(b) => b,
    };

    // Can we force the defender into letting us play in a critical
    // cell? We can send them anywhere our move lets us, but ignore
    // cells that are closed, or where the defender would win.
    let dests =
        pos.boards.free_squares(board) & !pos.game_states.donebits() & !(defender_critical as u32);
    // We want them to send us to one of these
    let want = pos.game_states.donebits() | (attacker_critical as u32);
    for i in 0..9 {
        if dests & (1 << i) == 0 {
            continue;
        }
        let send = pos.boards.free_squares(i);
        if send & !want == 0 {
            return true;
        }
    }

    return false;
}

fn forced_loss(pos: &game::Game, defender_critical: u16) -> bool {
    if defender_critical == 0 {
        return false;
    }
    // If the defender has a critial cell, and we have to send
    // them there, and we cannot block them, they win
    let board = match pos.board_to_play() {
        // If we can play on any board, assume we aren't forced
        None => return false,
        // If we can play in a critical board, assume we might be able
        // to block them
        Some(b) if defender_critical & (1 << b) != 0 => return false,
        Some(b) => b,
    };

    // We are forced to play in a specific board, and it
    // is not one of the opponent's critical boards. Let's
    // look at where we can send them
    let dests = pos.boards.free_squares(board);
    let bad = pos.game_states.donebits() | (defender_critical as u32);
    if dests & !bad == 0 {
        return true;
    }
    return false;
}

#[inline]
fn is_single_bit(v: u32) -> bool {
    debug_assert!(v != 0);
    v & (v - 1) == 0
}

#[inline]
fn critical_indices(player: u32, empty: u32) -> impl Iterator<Item = usize> {
    game::WIN_MASKS.iter().filter_map(move |mask| {
        if (player | empty) & mask != *mask {
            return None;
        }
        if !is_single_bit(empty & mask) {
            return None;
        }

        return Some((mask & empty).trailing_zeros() as usize);
    })
}

pub fn critical_boards(g: &game::Game, who: game::Player) -> u16 {
    let mut out = 0_u16;
    for board in critical_indices(g.game_states.playerbits(who), !g.game_states.donebits()) {
        if critical_indices(g.boards.playerbits(who, board), !g.boards.mask(board))
            .next()
            .is_some()
        {
            out |= 1_u16 << board;
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::game;
    use crate::game::notation::parse;
    use crate::minimax;

    #[test]
    fn test_is_winnable() {
        let tests = &[
            (game::Game::new(), true, true),
            (game::Game::new(), true, true),
            (
                parse("O;XOX.X.OX.;X.OXO.X../OX.O..O../XXX...XOO/..OX.OXO./OX..X.OX./...O.X.O./..X...OOO/O.OXXX.X./XOO....XX").unwrap(),
                true, false,
            ),
            (
                parse("X;O.OXX.XOX;XOOOO..O./XXOX.OOOX/XOXX.XOOO/.X.OXO.X./OXO.X..X./.XXO.X.OO/X..X..X.O/XOOXOXO../.O...OXXX").unwrap(),
                true, true,
            )
        ];
        for &(ref pos, expect_x, expect_o) in tests.iter() {
            assert_eq!(expect_x, is_winnable(pos, game::Player::X));
            assert_eq!(expect_o, is_winnable(pos, game::Player::O));
        }
    }

    #[test]
    fn test_is_single_bit() {
        let tests = &[
            (0x1_u32, true),
            (0x2_u32, true),
            (0x8000_u32, true),
            (0x8001_u32, false),
            (0x8003_u32, false),
            (1_u32 << 15, true),
            (1_u32 << 15 | 2, false),
            (1_u32 << 31, true),
            (1_u32 << 31 | 1_u32 << 16, false),
            (1_u32 << 31 | 0x3, false),
        ];
        for &(n, expect) in tests.iter() {
            assert_eq!(
                expect,
                is_single_bit(n),
                "is_single_bit(0x{:x}) != {}",
                n,
                expect
            );
        }
    }

    #[test]
    fn test_critical_cell() {
        let tests = &[
            ("X;OXO.O.@..;X.OOOO.O./XO.X.OXO./X.X.X.OOO/.XXOXO.OX/OXOOX.O../.XXXX..O./X.O...X.O/XO.X.XOXX/.O.O.O.XX", 0, 0x140),
            ("X;OXO.O.@..;X.OOOO.O./XO.X.OXO./X.X.X.OOO/.XXOXO.OX/OXOOX.O../.XXXX..O./X.O.X.X.O/XO.X.XOXX/.O.OXO.XX", 0, 0x40),
            ("X;OX..O@X..;X.OOOOXO./XO.X.OXO./X.XXXOOXO/XXOOXO.O./OXOOX.O../.XX...OOX/O.OXXXX.O/XOXX.XO../.OO....XX", 0, 0x100),
            ("X;OX.@OX..X;XOOOX.OOO/XXXX.O.OO/X.XO..O.O/XX.OXO.../OOOOX..../XXX....O./O...X.X.O/XO.X.X.../.O....XXX", 0x4, 0),
        ];
        for &(board, want_x, want_o) in tests {
            let pos = game::notation::parse(board).unwrap();
            let got_x = critical_boards(&pos, game::Player::X);
            let got_o = critical_boards(&pos, game::Player::O);
            assert_eq!(want_x, got_x, "critical_cell({}, X)", board);
            assert_eq!(want_o, got_o, "critical_cell({}, O)", board);
        }
    }

    #[test]
    fn test_endgame() {
        let tests = &[
            ("X;OX@.O.O.X;XOOOOXXOO/XO.X.OXO./X.X.X.O.O/XX.OXOX../OXOOX.O../OXX...OO./OOOOXXX.O/XOOX.X.X./XO....XXX",
             prove::Status::unproven(),
            ),
            ("O;OX..@XXX.;X.OOO.OO./XO.X.OXO./X.XXX.O.O/XXOOXOO../OXOOXO.O./XXX....O./X.OX..X.O/XO.XXX.../.O.....XX",
             prove::Status::x(),
            ),
            ("X;OXO..X@XX;XOOOOO.O./XXXXOO.OO/X.X...OOO/XX.OXO.../OXOOX..O./XXX....O./.O..X.X.O/XO.XXX.../.O....XXX",
             prove::Status::x(),
            ),
            ("O;OXO.@X.XX;XOOOOO.O./XXXXOO.OO/X.X...OOO/XX.OXO.../OXOOX..O./XXX....O./.O..X.X.O/XO.XXX.../.O....XXX",
             prove::Status::x(),
            ),
            ("O;OX@XOOOX.;XOOOOXXO./XO.X.OXO./X.X.X.OXO/XXXOXO.../OXOOXOXOO/OXXOOXOO./OOO.XXX.O/XO.XXXO../.O..XX.XX",
             prove::Status::draw_or_o(),
            ),
            ("X;O..XOX@X.;X.OOO.OO./XO.XXOOO./X.X..OO.O/.X.OXO.X./OXOOX.O../XXX....O./.XX...XX./XO.XXXOO./OO.....XX",
             prove::Status::o(),
            ),
        ];
        let mut mm = minimax::Minimax::with_config(&minimax::Config {
            max_depth: Some(10),
            timeout: None,
            debug: 0,
            table_bytes: None,
            draw_winner: None,
        });
        for &(board, expect) in tests {
            let pos = game::notation::parse(board).unwrap();
            let an = Analysis::new(&pos);
            let got = an.status();
            assert_eq!(
                expect, got,
                "prove(\"{}\")={:?} want {:?}",
                board, got, expect,
            );
            let (_, vstats) = mm.analyze(&pos);
            let score = vstats.last().unwrap().score;
            let mmeval = if score >= minimax::EVAL_WON {
                prove::Status::for_player(pos.player())
            } else if score <= minimax::EVAL_LOST {
                prove::Status::for_player(pos.player().other())
            } else {
                prove::Status::unproven()
            };
            assert!(
                expect.merge(mmeval).is_some(),
                "Result {:?} is incompatible with minimax result {:?}",
                got,
                mmeval,
            );
        }
    }
}
