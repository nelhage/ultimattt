#![feature(new_uninit)]
#![feature(asm)]
#![feature(trait_alias)]
#![feature(portable_simd)]
#![allow(incomplete_features)]
#![feature(adt_const_params)]

pub mod endgame;
pub mod game;
pub mod minimax;
pub mod progress;
pub mod protocol;
pub mod prove;
pub mod table;
pub mod util;
