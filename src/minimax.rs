extern crate test;
use crate::game;

use rand;
use std::time::{Duration, Instant};
use std::vec::Vec;

pub trait AI {
    fn select_move(&mut self, g: &game::Game) -> game::Move;
}

struct Stats {
    visited: i64,
    terminal: i64,
    cuts: i64,
}

impl Default for Stats {
    fn default() -> Self {
        Stats {
            visited: 0,
            terminal: 0,
            cuts: 0,
        }
    }
}

#[allow(dead_code)]
pub struct Minimax {
    rng: rand::rngs::ThreadRng,

    max_depth: Option<i32>,
    timeout: Option<Duration>,

    stats: Stats,
}

const EVAL_WON: i64 = 1 << 60;
const EVAL_LOST: i64 = -(1 << 60);
const EVAL_PARTIAL_ONE: i64 = 1;
const EVAL_PARTIAL_TWO: i64 = 3;

const OVERALL_PARTIAL_WIN: i64 = 10;

impl Minimax {
    #[allow(dead_code)]
    pub fn with_depth(depth: i32) -> Self {
        Self {
            rng: rand::thread_rng(),
            max_depth: Some(depth),
            timeout: None,
            stats: Default::default(),
        }
    }

    #[allow(dead_code)]
    pub fn with_timeout(timeout: Duration) -> Self {
        Self {
            rng: rand::thread_rng(),
            max_depth: None,
            timeout: Some(timeout),

            stats: Default::default(),
        }
    }

    fn score_board(&self, g: &game::Game, board: usize) -> i64 {
        if !g.game_states.in_play(board) {
            return 0;
        }
        struct WinPotentials {
            // (x, o)
            n1: (i64, i64),
            n2: (i64, i64),
        };
        let xbits = g.boards.xbits(board);
        let obits = g.boards.obits(board);
        let mut potentials = WinPotentials {
            n1: (0, 0),
            n2: (0, 0),
        };

        for mask in game::WIN_MASKS {
            let xs = xbits & mask;
            let os = obits & mask;

            // If neither player has a play, or both players have a
            // play, this is a dead line; assign no points
            if (xs == 0 && os == 0) || (xs != 0 && os != 0) {
                continue;
            }
            if xs != 0 {
                if xs.count_ones() == 1 {
                    potentials.n1.0 += 1;
                } else if xs.count_ones() == 2 {
                    potentials.n2.0 += 1;
                }
            } else {
                if os.count_ones() == 1 {
                    potentials.n1.1 += 1;
                } else if os.count_ones() == 2 {
                    potentials.n2.1 += 1;
                }
            }
        }
        let d1 = EVAL_PARTIAL_ONE * (potentials.n1.0 - potentials.n1.1);
        let d2 = EVAL_PARTIAL_TWO * (potentials.n2.0 - potentials.n2.1);
        return d1 + d2;
    }

    fn evaluate(&self, g: &game::Game) -> i64 {
        match g.game_state() {
            game::BoardState::Drawn => (),
            game::BoardState::InPlay => (),
            game::BoardState::Won(p) => return if p == g.player() { EVAL_WON } else { EVAL_LOST },
        };

        let mut board_scores: [i64; 9] = [0; 9];
        for board in 0..9 {
            board_scores[board] = self.score_board(g, board);
        }
        let mut score: i64 = 0;
        for (i, mask) in game::WIN_MASKS.iter().enumerate() {
            let xbits = g.game_states.xbits() & mask;
            let obits = g.game_states.obits() & mask;

            // If both players have a play, this is a dead line. Award
            // no points.
            if xbits != 0 && obits != 0 {
                continue;
            }
            score += (xbits.count_ones() as i64) * OVERALL_PARTIAL_WIN;
            score -= (obits.count_ones() as i64) * OVERALL_PARTIAL_WIN;

            for idx in game::WIN_PATTERNS[i].iter() {
                score += board_scores[*idx];
            }
        }

        if g.player() == game::Player::O {
            -score
        } else {
            score
        }
    }

    fn minimax(
        &mut self,
        g: &game::Game,
        depth: i32,
        mut alpha: i64,
        beta: i64,
        pv: &mut [game::Move],
    ) -> i64 {
        self.stats.visited += 1;
        if depth <= 0 || g.game_state().terminal() {
            self.stats.terminal += 1;
            return self.evaluate(g);
        }

        let mut moves = g.all_moves();
        if !pv[0].is_none() {
            let idx = moves.iter().position(|m| *m == pv[0]);
            if let Some(i) = idx {
                moves.swap(0, i);
            }
        }
        for m in moves {
            let child = g.make_move(m).unwrap();
            let score = -self.minimax(
                &child,
                depth - 1,
                -beta,
                -alpha,
                &mut pv[1..(depth as usize)],
            );
            if score > alpha {
                alpha = score;
                pv[0] = m;
                if alpha >= beta {
                    self.stats.cuts += 1;
                    break;
                }
            }
        }
        alpha
    }

    fn format_pv(&self, pv: &[game::Move]) -> String {
        let mut s = "[".to_owned();
        for m in pv {
            s += &game::notation::render_move(*m);
            s += " ";
        }
        s.pop();
        s += "]";
        return s;
    }

    fn search(&mut self, g: &game::Game) -> (Vec<game::Move>, i64) {
        let mut pv = Vec::new();

        let deadline: Option<Instant> = self.timeout.map(|t| Instant::now() + t);
        let mut depth: i32 = 0;
        let mut score;
        loop {
            depth += 1;
            let t_before = Instant::now();
            self.stats = Default::default();
            pv.resize(depth as usize, game::Move::none());
            score = self.minimax(g, depth, EVAL_LOST, EVAL_WON, pv.as_mut_slice());
            let ply_duration = Instant::now().duration_since(t_before);
            println!(
                "minimax depth={} move={} pv={} v={} t={}.{:03}s visited={} cuts={}",
                depth,
                pv[0],
                self.format_pv(&pv),
                score,
                ply_duration.as_secs(),
                ply_duration.subsec_millis(),
                self.stats.visited,
                self.stats.cuts,
            );
            if self.max_depth.map(|d| depth >= d).unwrap_or(false) {
                break;
            }
            if deadline.map(|d| Instant::now() > d).unwrap_or(false) {
                break;
            }
        }
        return (pv, score);
    }
}

impl AI for Minimax {
    fn select_move(&mut self, g: &game::Game) -> game::Move {
        let (pv, _) = self.search(g);
        pv[0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[test]
    fn test_eval_empty() {
        let g = game::Game::new();
        let ai = Minimax::with_depth(3);
        assert_eq!(0, ai.evaluate(&g));
    }

    #[test]
    fn test_eval_subboard() {
        let ai = Minimax::with_depth(3);
        let tests: &[(&'static str, i64)] = &[
            ("X;.@.......;...X...../........./........./.O......./........./........./........./........./.........", 2),
            ("X;.@.......;XX......./........./........./.O......./........./........./........./........./.........", 6),
            ("X;.@.......;XX..O..../........./........./.O......./........./........./........./........./.........", 2),
            ("X;.@.......;XX..X..../........./........./.O......./........./........./........./........./.........", 12),
            ("X;.@.......;XX..X...O/........./........./.O......./........./........./........./........./.........", 7),
        ];
        for (i, tc) in tests.iter().enumerate() {
            let g = game::notation::parse(tc.0).unwrap();
            assert_eq!(ai.score_board(&g, 0), tc.1, "test#{}: {}", i, tc.0);
        }
    }

    #[test]
    fn test_minimax_basic() {
        let g = game::Game::new()
            .make_move(game::Move::from_coords(0, 5))
            .unwrap();
        let mut ai = Minimax::with_depth(5);
        ai.select_move(&g);
    }

    #[bench]
    fn bench_evaluate(b: &mut Bencher) {
        use std::hint::black_box;
        let g = game::Game::new();
        let ai = Minimax::with_depth(3);
        b.iter(|| ai.evaluate(black_box(&g)));
    }
}
