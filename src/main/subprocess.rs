use serde_json;
use std::io;
use std::io::{BufRead, Write};
use std::process;
use ultimattt::game;
use ultimattt::minimax;
use ultimattt::protocol;

pub struct Player {
    config: minimax::Config,
    child: Box<process::Child>,
    stdout: io::BufReader<process::ChildStdout>,
}

impl Player {
    pub fn new(
        args: Vec<String>,
        player: game::Player,
        config: &minimax::Config,
    ) -> Result<Self, io::Error> {
        let mut iter = args.into_iter();
        let mut child = Box::new(
            process::Command::new(iter.next().unwrap())
                .args(iter)
                .stdin(process::Stdio::piped())
                .stdout(process::Stdio::piped())
                .spawn()?,
        );

        let mut out = Player {
            config: config.clone(),
            stdout: io::BufReader::new(child.stdout.take().unwrap()),
            child: child,
        };
        match out.send_command(&protocol::Command::NewGame {
            id: "game0".to_owned(),
            player: player.as_str().to_owned(),
        })? {
            protocol::Response::Ok(..) => Ok(out),
            got => Err(io::Error::new(
                io::ErrorKind::Other,
                format!("bad response to NewGame command: {:?}", got),
            )),
        }
    }

    fn send_command(&mut self, cmd: &protocol::Command) -> io::Result<protocol::Response> {
        let json = serde_json::to_string(cmd)?;
        writeln!(self.child.stdin.as_mut().unwrap(), "{}", json)?;
        let mut gotjson = String::new();
        self.stdout.read_line(&mut gotjson)?;
        let got: protocol::Response = serde_json::from_str(&gotjson)?;
        match got {
            protocol::Response::Err { error } => Err(io::Error::new(io::ErrorKind::Other, error)),
            _ => Ok(got),
        }
    }
}

impl Drop for Player {
    fn drop(&mut self) {
        match self.send_command(&protocol::Command::Shutdown()) {
            _ => (),
        }
    }
}

impl minimax::AI for Player {
    fn select_move(&mut self, g: &game::Game) -> Result<game::Move, minimax::Error> {
        match self
            .send_command(&protocol::Command::GetMove {
                id: "game0".to_owned(),
                board: game::notation::render(g),
                limit: self.config.limit,
                max_depth: self.config.max_depth,
            })
            .unwrap()
        {
            protocol::Response::Move { ref move_ } => game::notation::parse_move(move_)
                .map_err(|e| minimax::Error::Other(format!("parsing move: {:?}", e))),
            other => Err(minimax::Error::Other(format!(
                "subprocess error: {:?}",
                other
            ))),
        }
    }
}
