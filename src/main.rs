mod game;

fn main() {
    let g = game::Game::new();
    match g.make_move(game::Move {
        board: 0,
        square: 0,
    }) {
        Ok(gg) => {
            println!("move ok!\n{}", gg);
        }
        Err(e) => {
            println!("bad move: {:?}", e);
        }
    };
}
