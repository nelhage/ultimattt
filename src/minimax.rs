extern crate smallvec;
use crate::game;

use rand;
use smallvec::SmallVec;
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

#[derive(Clone)]
pub struct Config {
    pub max_depth: Option<i32>,
    pub timeout: Option<Duration>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            max_depth: None,
            timeout: None,
        }
    }
}

struct ResponseTable {
    moves: [game::Move; 256],
}

impl Default for ResponseTable {
    fn default() -> Self {
        Self {
            moves: [game::Move::none(); 256],
        }
    }
}

#[allow(dead_code)]
pub struct Minimax {
    rng: rand::rngs::ThreadRng,

    config: Config,
    stats: Stats,
    response: [ResponseTable; 2],
}

const EVAL_WON: i64 = 1 << 60;
const EVAL_LOST: i64 = -(1 << 60);
const EVAL_PARTIAL_ONE: i64 = 1;
const EVAL_PARTIAL_TWO: i64 = 3;

const OVERALL_PARTIAL_WIN: i64 = 10;

struct DedupIterator<T>
where
    T: Iterator<Item = game::Move>,
{
    seen: [u64; 4],
    inner: T,
}

impl<T> DedupIterator<T>
where
    T: Iterator<Item = game::Move>,
{
    fn new(inner: T) -> Self {
        Self {
            seen: [0; 4],
            inner: inner,
        }
    }
}

impl<T> Iterator for DedupIterator<T>
where
    T: Iterator<Item = game::Move>,
{
    type Item = game::Move;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let n = self.inner.next();
            if let Some(ref e) = n {
                let idx: usize = (e.bits() / 64) as usize;
                let bit = 1 << (e.bits() % 64);
                if self.seen[idx] & bit != 0 {
                    continue;
                }
                self.seen[idx] |= bit;
            }
            return n;
        }
    }
}

#[allow(dead_code)]
impl Minimax {
    pub fn with_config(config: &Config) -> Self {
        Self {
            rng: rand::thread_rng(),
            config: config.clone(),
            stats: Default::default(),
            response: Default::default(),
        }
    }

    pub fn with_timeout(timeout: Duration) -> Self {
        Self::with_config(&Config {
            timeout: Some(timeout),
            ..Default::default()
        })
    }

    pub fn with_depth(depth: i32) -> Self {
        Self::with_config(&Config {
            max_depth: Some(depth),
            ..Default::default()
        })
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

    pub fn evaluate(&self, g: &game::Game) -> i64 {
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

    fn move_iterator<'a>(
        &mut self,
        g: &'a game::Game,
        pv: game::Move,
        prev: game::Move,
    ) -> impl Iterator<Item = game::Move> + 'a {
        let mut ordered: SmallVec<[game::Move; 2]> = SmallVec::new();
        let all_moves = g.all_moves();

        let refutation = self.response_to(g.player().other(), prev);
        if refutation.is_some() {
            ordered.push(refutation);
        } else if pv.is_some() {
            ordered.push(pv);
        }
        DedupIterator::new(ordered.into_iter().chain(all_moves))
    }

    fn minimax(
        &mut self,
        g: &game::Game,
        depth: i32,
        mut alpha: i64,
        beta: i64,
        pv: &mut [game::Move],
        prev: game::Move,
    ) -> i64 {
        self.stats.visited += 1;
        if depth <= 0 || g.game_state().terminal() {
            self.stats.terminal += 1;
            return self.evaluate(g);
        }
        let mut localpv: SmallVec<[game::Move; 10]> =
            SmallVec::from_elem(game::Move::none(), (depth - 1) as usize);

        let moves = self.move_iterator(g, pv[0], prev);
        for m in moves {
            let child = match g.make_move(m) {
                Ok(g) => g,
                Err(_) => continue,
            };
            let score = if pv[0].is_some() {
                let zw = -self.minimax(
                    &child,
                    depth - 1,
                    -alpha - 1,
                    -alpha,
                    localpv.as_mut_slice(),
                    m,
                );
                if zw > alpha && zw < beta {
                    -self.minimax(&child, depth - 1, -beta, -alpha, localpv.as_mut_slice(), m)
                } else {
                    zw
                }
            } else {
                -self.minimax(&child, depth - 1, -beta, -alpha, localpv.as_mut_slice(), m)
            };
            if score > alpha {
                alpha = score;
                pv[0] = m;
                pv[1..(depth as usize)].copy_from_slice(&localpv);
                if alpha >= beta {
                    self.set_response(g.player().other(), prev, m);
                    self.stats.cuts += 1;
                    break;
                }
            }
        }
        alpha
    }

    fn response_to(&self, player: game::Player, m: game::Move) -> game::Move {
        let i = match player {
            game::Player::X => 0,
            game::Player::O => 1,
        };
        self.response[i].moves[m.bits() as usize]
    }
    fn set_response(&mut self, player: game::Player, m: game::Move, resp: game::Move) {
        let i = match player {
            game::Player::X => 0,
            game::Player::O => 1,
        };
        self.response[i].moves[m.bits() as usize] = resp;
    }

    fn format_pv(&self, pv: &[game::Move]) -> String {
        let mut s = "[".to_owned();
        for m in pv {
            if m.is_none() {
                break;
            }
            s += &game::notation::render_move(*m);
            s += " ";
        }
        s.pop();
        s += "]";
        return s;
    }

    fn search(&mut self, g: &game::Game) -> (Vec<game::Move>, i64) {
        let mut pv = Vec::new();

        let t_start = Instant::now();
        let deadline: Option<Instant> = self.config.timeout.map(|t| t_start + t);
        let mut depth: i32 = 0;
        let mut score;
        loop {
            depth += 1;
            let t_before = Instant::now();
            self.stats = Default::default();
            pv.resize(depth as usize, game::Move::none());
            score = self.minimax(
                g,
                depth,
                EVAL_LOST,
                EVAL_WON,
                pv.as_mut_slice(),
                game::Move::none(),
            );
            let duration = Instant::now().duration_since(t_start);
            let ply_duration = Instant::now().duration_since(t_before);
            let m_ms = (self.stats.visited as f64) / (1000.0 * ply_duration.as_secs_f64());
            println!(
                "minimax depth={} move={} v={} t={}.{:03}s({}.{:03}s) m/ms={:.3} visited={} cuts={}",
                depth,
                pv[0],
                score,
                ply_duration.as_secs(),
                ply_duration.subsec_millis(),
                duration.as_secs(),
                duration.subsec_millis(),
                m_ms,
                self.stats.visited,
                self.stats.cuts,
            );
            println!("  pv={}", self.format_pv(&pv),);
            if self.config.max_depth.map(|d| depth >= d).unwrap_or(false) {
                break;
            }
            if deadline.map(|d| Instant::now() > d).unwrap_or(false) {
                break;
            }
            if score <= EVAL_LOST || score >= EVAL_WON {
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

    #[test]
    fn test_minimax_pv() {
        let g = game::Game::new();
        let mut ai = Minimax::with_depth(6);
        let (pv, _) = ai.search(&g);
        pv.iter().fold(g, |g, m| {
            let n = g.make_move(*m);
            match n {
                Ok(gg) => gg,
                Err(e) => panic!("make move({}): {:?}", game::notation::render_move(*m), e),
            }
        });
    }
}
