use crate::game;

pub fn is_winnable(g: &game::Game, by: game::Player) -> bool {
    let potential = g.game_states.playerbits(by) | !g.game_states.donebits();
    for &mask in game::WIN_MASKS.iter() {
        if potential & mask == mask {
            return true;
        }
    }
    return false;
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
}
