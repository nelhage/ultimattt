use rand;
use rand::seq::IteratorRandom;
use rand::Rng;
use ultimattt::{game, minimax};

pub struct Player<'a> {
    rng: rand::rngs::ThreadRng,
    epsilon: f64,
    player: Box<dyn minimax::AI + 'a>,
}

impl<'a> Player<'a> {
    pub fn new(inner: Box<dyn minimax::AI + 'a>, epsilon: f64) -> Self {
        Self {
            rng: rand::thread_rng(),
            epsilon: epsilon,
            player: inner,
        }
    }
}

impl<'a> minimax::AI for Player<'a> {
    fn select_move(&mut self, g: &game::Game) -> Result<game::Move, minimax::Error> {
        let inner = self.player.select_move(g)?;
        if self.rng.gen::<f64>() < self.epsilon {
            let all_moves = g.all_moves();
            match all_moves.choose(&mut self.rng) {
                None => Err(minimax::Error::Other("no legal moves".to_owned())),
                Some(m) => Ok(m),
            }
        } else {
            Ok(inner)
        }
    }
}
