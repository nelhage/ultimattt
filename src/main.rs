mod game;

fn main() {
    let g = game::Game::new();
    match g.make_move(game::Move {
        board: 0,
        square: 0,
    }) {
        Ok(_) => {
            println!("move ok!");
        }
        Err(e) => {
            println!("bad move: {:?}", e);
        }
    }
}
