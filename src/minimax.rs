use crate::game;

use rand;
use rand::seq::SliceRandom;

pub struct AI {
    rng: rand::rngs::ThreadRng,
}

impl AI {
    pub fn new() -> Self {
        AI {
            rng: rand::thread_rng(),
        }
    }

    pub fn select_move(&mut self, g: &game::Game) -> game::Move {
        let moves = g.all_moves();
        *moves.as_slice().choose(&mut self.rng).unwrap()
    }
}
