extern crate vergen;

use vergen::{generate_cargo_keys, ConstantsFlags};

fn main() {
    let flags = ConstantsFlags::SHA | ConstantsFlags::REBUILD_ON_HEAD_CHANGE;
    // Generate the 'cargo:' key output
    generate_cargo_keys(flags).expect("Unable to generate the cargo keys!");
}
