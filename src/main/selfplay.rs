use std::io;
use ultimattt::{game, minimax};

pub struct Executor<'a, F1, F2>
where
    F1: Fn(game::Player) -> Result<Box<dyn minimax::AI + 'a>, io::Error>,
    F2: Fn(game::Player) -> Result<Box<dyn minimax::AI + 'a>, io::Error>,
{
    player1: F1,
    player2: F2,
}

#[derive(Default)]
pub struct Results {
    // (X wins, O wins, draw)
    p1_x: (usize, usize, usize),
    p1_o: (usize, usize, usize),
    pub games: usize,
}

#[allow(dead_code)]
impl Results {
    pub fn player1_wins(&self) -> usize {
        self.p1_x.0 + self.p1_o.1
    }
    pub fn player2_wins(&self) -> usize {
        self.p1_x.1 + self.p1_o.0
    }
    pub fn draws(&self) -> usize {
        self.p1_x.2 + self.p1_o.2
    }

    pub fn p1_x_results(&self) -> (usize, usize, usize) {
        self.p1_x
    }

    pub fn p1_o_results(&self) -> (usize, usize, usize) {
        self.p1_o
    }

    pub fn p2_x_results(&self) -> (usize, usize, usize) {
        (self.p1_o.1, self.p1_o.0, self.p1_o.2)
    }

    pub fn p2_o_results(&self) -> (usize, usize, usize) {
        (self.p1_x.1, self.p1_x.0, self.p1_x.2)
    }
}

impl<'a, F1, F2> Executor<'a, F1, F2>
where
    F1: Fn(game::Player) -> Result<Box<dyn minimax::AI + 'a>, io::Error>,
    F2: Fn(game::Player) -> Result<Box<dyn minimax::AI + 'a>, io::Error>,
{
    pub fn new(player1: F1, player2: F2) -> Self {
        Self {
            player1: player1,
            player2: player2,
        }
    }

    pub fn execute(&mut self, games: usize) -> Result<Results, io::Error> {
        let mut results: Results = Default::default();
        for i in 0..games {
            let (mut p1, mut p2, dst) = if i % 2 == 0 {
                (
                    (self.player1)(game::Player::X)?,
                    (self.player2)(game::Player::O)?,
                    &mut results.p1_x,
                )
            } else {
                (
                    (self.player2)(game::Player::X)?,
                    (self.player1)(game::Player::O)?,
                    &mut results.p1_o,
                )
            };
            let result = self.simulate(&mut *p1, &mut *p2)?;
            match result {
                game::GameState::Drawn => dst.2 += 1,
                game::GameState::InPlay => panic!("impossible"),
                game::GameState::Won(p) => {
                    if p == game::Player::X {
                        dst.0 += 1;
                    } else {
                        dst.1 += 1;
                    }
                }
            }
            results.games += 1;
        }
        Ok(results)
    }

    fn simulate(
        &self,
        player_x: &mut (dyn minimax::AI + 'a),
        player_o: &mut (dyn minimax::AI + 'a),
    ) -> Result<game::GameState, io::Error> {
        let mut board = game::Game::new();
        while let game::GameState::InPlay = board.game_state() {
            let m = match board.player() {
                game::Player::X => player_x.select_move(&board),
                game::Player::O => player_o.select_move(&board),
            }
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("reading move: {:?}", e)))?;
            board = board.make_move(m).map_err(|e| {
                io::Error::new(
                    io::ErrorKind::Other,
                    format!(
                        "illegal move: ({}): {}: {:?}",
                        game::notation::render(&board),
                        game::notation::render_move(m),
                        e
                    ),
                )
            })?;
        }
        Ok(board.game_state())
    }
}
