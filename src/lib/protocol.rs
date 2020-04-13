extern crate serde;
use serde::{Deserialize, Serialize};

use std::time::Duration;

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "op", content = "body", rename_all = "lowercase")]
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
    Shutdown(),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "op", content = "body", rename_all = "lowercase")]
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
