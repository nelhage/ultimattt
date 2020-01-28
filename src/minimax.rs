use crate::game;

use rand;
use rand::seq::SliceRandom;

pub trait AI {
    fn select_move(&mut self, g: &game::Game) -> game::Move;
}

pub struct Minimax {
    rng: rand::rngs::ThreadRng,
}

impl Minimax {
    pub fn new() -> Self {
        Self {
            rng: rand::thread_rng(),
        }
    }
}

impl AI for Minimax {
    fn select_move(&mut self, g: &game::Game) -> game::Move {
        let moves = g.all_moves();
        *moves.as_slice().choose(&mut self.rng).unwrap()
    }
}
