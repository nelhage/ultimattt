use super::Opt;

use std::io;
use ultimattt::game::notation;
use ultimattt::minimax;
use ultimattt::minimax::AI;
use ultimattt::protocol;

pub struct Worker<'a> {
    #[allow(dead_code)]
    opt: &'a Opt,
    stdin: &'a mut dyn io::Read,
    stdout: &'a mut dyn io::Write,

    ai: Option<minimax::Minimax>,
}

impl<'a> Worker<'a> {
    pub fn new(stdin: &'a mut dyn io::Read, stdout: &'a mut dyn io::Write, opt: &'a Opt) -> Self {
        Worker {
            stdin: stdin,
            stdout: stdout,
            opt: opt,
            ai: None,
        }
    }

    pub fn run(&mut self) -> Result<(), io::Error> {
        loop {
            let cmd: protocol::Command = serde_json::from_reader(&mut self.stdin)?;
            let resp = self.handle_command(&cmd);
            serde_json::to_writer(&mut self.stdout, &resp)?;
        }
    }

    fn handle_command(&mut self, cmd: &protocol::Command) -> protocol::Response {
        match cmd {
            protocol::Command::NewGame { .. } => protocol::Response::Ok(),
            protocol::Command::GetMove {
                id: _,
                ref board,
                ref limit,
                ref max_depth,
            } => {
                let board = match notation::parse(&board) {
                    Ok(b) => b,
                    Err(e) => {
                        return protocol::Response::Err {
                            error: format!("bad notation: {:?}", e),
                        };
                    }
                };
                let config = minimax::Config {
                    max_depth: *max_depth,
                    timeout: *limit,
                    ..Default::default()
                };
                let ai = self.get_ai(&config);
                let m = ai.select_move(&board);
                protocol::Response::Move {
                    move_: notation::render_move(m),
                }
            }
        }
    }

    fn get_ai(&mut self, cfg: &minimax::Config) -> &mut minimax::Minimax {
        if self
            .ai
            .as_ref()
            .map(|ai| ai.config() != cfg)
            .unwrap_or(true)
        {
            self.ai = Some(minimax::Minimax::with_config(cfg));
        }
        self.ai.as_mut().unwrap()
    }
}
