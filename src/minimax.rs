use crate::game;

use rand;
use std::time::{Duration, Instant};

pub trait AI {
    fn select_move(&mut self, g: &game::Game) -> game::Move;
}

pub struct Minimax {
    #[allow(dead_code)]
    rng: rand::rngs::ThreadRng,

    max_depth: Option<i32>,
    timeout: Option<Duration>,
}

const EVAL_WON: i64 = 1 << 60;
const EVAL_LOST: i64 = -(1 << 60);

impl Minimax {
    #[allow(dead_code)]
    pub fn with_depth(depth: i32) -> Self {
        Self {
            rng: rand::thread_rng(),
            max_depth: Some(depth),
            timeout: None,
        }
    }

    #[allow(dead_code)]
    pub fn with_timeout(timeout: Duration) -> Self {
        Self {
            rng: rand::thread_rng(),
            max_depth: None,
            timeout: Some(timeout),
        }
    }

    fn evaluate(&self, g: &game::Game) -> i64 {
        match g.game_state() {
            game::BoardState::Drawn => 0,
            game::BoardState::InPlay => 0,
            game::BoardState::Won(p) => return if p == g.player() { EVAL_WON } else { EVAL_LOST },
        };

        let mut wins = (0, 0);
        for board in 0..9 {
            match g.board_state(board) {
                game::BoardState::Won(p) => {
                    if p == game::Player::X {
                        wins.0 += 1
                    } else {
                        wins.1 += 1
                    }
                }
                _ => (),
            }
        }

        let score: i64 = wins.0 - wins.1;
        if g.player() == game::Player::O {
            -score
        } else {
            score
        }
    }

    fn minimax(&mut self, g: &game::Game, depth: i32) -> (game::Move, i64) {
        if depth <= 0 {
            return (game::Move::none(), self.evaluate(g));
        }

        let mut best = (game::Move::none(), EVAL_LOST - 1);
        let moves = g.all_moves();
        for m in moves {
            let child = g.make_move(m).unwrap();
            let mut eval = self.minimax(&child, depth - 1);
            eval.1 *= -1;
            if eval.1 > best.1 {
                best = (m, eval.1)
            }
        }
        best
    }
}

impl AI for Minimax {
    fn select_move(&mut self, g: &game::Game) -> game::Move {
        let deadline: Option<Instant> = self.timeout.map(|t| Instant::now() + t);
        let mut depth = 0;
        let mut result: game::Move;
        loop {
            depth += 1;
            let t_before = Instant::now();
            let got = self.minimax(g, depth);
            let ply_duration = Instant::now().duration_since(t_before);
            result = got.0;
            println!(
                "minimax depth={} move={} v={} t={}.{:03}s",
                depth,
                got.0,
                got.1,
                ply_duration.as_secs(),
                ply_duration.subsec_millis(),
            );
            if self.max_depth.map(|d| depth >= d).unwrap_or(false) {
                break;
            }
            if deadline.map(|d| Instant::now() > d).unwrap_or(false) {
                break;
            }
        }
        return result;
    }
}
