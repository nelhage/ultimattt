extern crate criterion;
extern crate ultimattt;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use ultimattt::game::{Game, Move};
use ultimattt::minimax::Minimax;

fn bench_empty_move(c: &mut Criterion) {
    c.bench_function("move(ae)", |b| {
        let g = Game::new();
        b.iter(|| {
            let gg = g.make_move(Move::from_coords(0, 5));
            black_box(&gg);
        })
    });
}

fn bench_recalc_winner_empty(c: &mut Criterion) {
    c.bench_function("recalc_winner", |b| {
        let mut g = Game::new();
        b.iter(|| {
            g.recalc_winner();
        });
    });
}

fn bench_clone(c: &mut Criterion) {
    c.bench_function("Game::clone", |b| {
        let g = Game::new();
        b.iter(|| {
            let gg = g.clone();
            black_box(&gg);
        });
    });
}

criterion_group!(
    game,
    bench_empty_move,
    bench_recalc_winner_empty,
    bench_clone,
);

fn bench_evaluate(c: &mut Criterion) {
    c.bench_function("evaluate", |b| {
        let g = Game::new();
        let ai = Minimax::with_depth(3);
        b.iter(|| ai.evaluate(black_box(&g)));
    });
}
criterion_group!(minimax, bench_evaluate);
criterion_main!(game, minimax);
