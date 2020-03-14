extern crate serde;
use serde::{Deserialize, Serialize};

use std::time::Duration;

#[derive(Serialize, Deserialize)]
#[serde(tag = "op", content = "body")]
pub enum Command {
    NewGame {
        id: String,
        player: String,
    },
    GetMove {
        id: String,
        board: String,
        limit: Option<Duration>,
        max_depth: Option<i64>,
    },
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "op", content = "body")]
pub enum Response {
    Ok(),
    Err {
        error: String,
    },
    Move {
        #[serde(rename = "move")]
        move_: String,
    },
}
