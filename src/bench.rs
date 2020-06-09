use ultimattt;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use std::mem;
use ultimattt::game::{Game, Move};
use ultimattt::minimax::Minimax;

fn bench_game(c: &mut Criterion) {
    eprintln!(
        "sizeof(Game)={} alignof(Game)={}",
        mem::size_of::<Game>(),
        mem::align_of::<Game>()
    );

    c.bench_function("move(ae)", |b| {
        let g = Game::new();
        b.iter(|| {
            let gg = g.make_move(Move::from_coords(0, 5));
            black_box(&gg);
        })
    });
    c.bench_function("recalc_winner", |b| {
        let mut g = Game::new();
        b.iter(|| {
            g.recalc_winner();
        });
    });
    c.bench_function("Game::clone", |b| {
        let g = Game::new();
        b.iter(|| {
            let gg = g.clone();
            black_box(&gg);
        });
    });

    c.bench_function("Game::zobrist", |b| {
        let g = Game::new();

        b.iter(|| {
            black_box(g.zobrist());
        });
    });
}

criterion_group!(game, bench_game,);

fn bench_evaluate(c: &mut Criterion) {
    c.bench_function("evaluate", |b| {
        let g = Game::new();
        let ai = Minimax::with_depth(3);
        b.iter(|| ai.evaluate(black_box(&g)));
    });
}

criterion_group!(minimax, bench_evaluate);
criterion_main!(game, minimax);
