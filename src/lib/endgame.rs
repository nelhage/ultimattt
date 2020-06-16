use crate::game;

use packed_simd::u16x8;

pub fn is_winnable(g: &game::Game, by: game::Player) -> bool {
    let potential = g.game_states.playerbits(by) | !g.game_states.donebits();
    return (u16x8::splat(potential as u16) & game::WIN_MASKS_SIMD)
        .eq(game::WIN_MASKS_SIMD)
        .any();
}

fn is_single_bit(v: u32) -> bool {
    debug_assert!(v != 0);
    v & (v - 1) == 0
}

fn critical_index(player: u32, empty: u32) -> Option<usize> {
    // let mut board: Option<usize> = None;
    for &mask in game::WIN_MASKS.iter() {
        /*
        eprintln!(
            "mask={:x} player={:x} empty={:x} maybe={}",
            mask,
            player,
            empty,
            (player | empty) & mask != mask,
        );
        */
        if (player | empty) & mask != mask {
            continue;
        }
        if !is_single_bit(empty & mask) {
            continue;
        }

        return Some((mask & empty).trailing_zeros() as usize);
    }
    None
}

pub fn critical_cell(g: &game::Game, who: game::Player) -> Option<(usize, usize)> {
    let board = critical_index(g.game_states.playerbits(who), !g.game_states.donebits())?;
    let cell = critical_index(g.boards.playerbits(who, board), !g.boards.mask(board))?;
    Some((board, cell))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::game;
    use crate::game::notation::parse;

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
            ("X;OXO.O.@..;X.OOOO.O./XO.X.OXO./X.X.X.OOO/.XXOXO.OX/OXOOX.O../.XXXX..O./X.O...X.O/XO.X.XOXX/.O.O.O.XX", None, Some((8, 4))),
            ("X;OXO.O.@..;X.OOOO.O./XO.X.OXO./X.X.X.OOO/.XXOXO.OX/OXOOX.O../.XXXX..O./X.O.X.X.O/XO.X.XOXX/.O.OXO.XX", None, None),
            ("X;OX..O@X..;X.OOOOXO./XO.X.OXO./X.XXXOOXO/XXOOXO.O./OXOOX.O../.XX...OOX/O.OXXXX.O/XOXX.XO../.OO....XX", None, Some((8, 0))),
            ("X;OX.@OX..X;XOOOX.OOO/XXXX.O.OO/X.XO..O.O/XX.OXO.../OOOOX..../XXX....O./O...X.X.O/XO.X.X.../.O....XXX", Some((2, 1)), None),
        ];
        for &(board, want_x, want_o) in tests {
            let pos = game::notation::parse(board).unwrap();
            let got_x = critical_cell(&pos, game::Player::X);
            let got_o = critical_cell(&pos, game::Player::O);
            assert_eq!(want_x, got_x, "critical_cell({}, X)", board);
            assert_eq!(want_o, got_o, "critical_cell({}, O)", board);
        }
    }
}
