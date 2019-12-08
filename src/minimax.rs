use crate::game;

use rand::seq::SliceRandom;
use rand::thread_rng;

pub fn minimax(g: &game::Game) -> game::Move {
    let moves = g.all_moves();
    let mut rng = thread_rng();
    *moves.as_slice().choose(&mut rng).unwrap()
}
