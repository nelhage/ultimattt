#![allow(dead_code)]
use bit_set::BitSet;
use packed_simd::u16x8;
use std::time::Instant;

const WIN_MASKS_SIMD: u16x8 = u16x8::new(0x7, 0x38, 0x1c0, 0x49, 0x92, 0x124, 0x111, 0x54);

#[derive(Copy, Clone, Debug)]
struct Game {
    x: u16,
    o: u16,
}

enum Winner {
    X,
    O,
    Draw,
    None,
}

impl Game {
    fn winner(self) -> Winner {
        if (u16x8::splat(self.x) & WIN_MASKS_SIMD)
            .eq(WIN_MASKS_SIMD)
            .any()
        {
            Winner::X
        } else if (u16x8::splat(self.o) & WIN_MASKS_SIMD)
            .eq(WIN_MASKS_SIMD)
            .any()
        {
            Winner::O
        } else if self.count() == 9 {
            Winner::Draw
        } else {
            Winner::None
        }
    }

    fn as_index(self) -> usize {
        (self.x | (self.o << 9)) as usize
    }

    fn empty(self, idx: usize) -> bool {
        (self.o | self.x) & (1_u16 << idx) == 0
    }

    fn count(self) -> usize {
        (self.o | self.x).count_ones() as usize
    }
    fn count_x(self) -> usize {
        self.x.count_ones() as usize
    }
    fn count_o(self) -> usize {
        self.o.count_ones() as usize
    }

    fn make_move(self, idx: usize, who: bool) -> Game {
        if who {
            Game {
                x: self.x | (1_u16 << idx),
                o: self.o,
            }
        } else {
            Game {
                x: self.x,
                o: self.o | (1_u16 << idx),
            }
        }
    }
}

#[derive(Default, Clone, Copy)]
struct Counts {
    visited: usize,
    x: usize,
    o: usize,
}

struct Brute {
    stats: [[Counts; 8]; 8],
    seen: bit_set::BitSet,
}

impl Brute {
    fn search(&mut self, g: Game) {
        if self.seen.contains(g.as_index()) {
            return;
        }
        self.seen.insert(g.as_index());
        let stats = &mut self.stats[g.count_x()][g.count_o()];
        stats.visited += 1;
        match g.winner() {
            Winner::X => {
                stats.x += 1;
                return;
            }
            Winner::O => {
                stats.o += 1;
                return;
            }
            Winner::Draw => {
                return;
            }
            _ => (),
        };

        for i in 0..9 {
            if g.empty(i) {
                self.search(g.make_move(i, false));
                self.search(g.make_move(i, true));
            }
        }
    }
}

fn main() {
    let mut brute = Brute {
        stats: Default::default(),
        seen: BitSet::new(),
    };
    let t = Instant::now();
    brute.search(Game { x: 0, o: 0 });
    let e = Instant::now().duration_since(t);
    println!("time:     {}.{:03}", e.as_secs(), e.subsec_millis());
    let mut total = 0;
    for (x, row) in brute.stats.iter().enumerate() {
        print!("x={}", x);
        for (_o, e) in row.iter().enumerate() {
            print!("  {:3}/{:-3}/{:3}|", e.x, e.o, e.visited);
            total += e.visited;
        }
        println!();
        /*
        let (x, o, v) = e
            .iter()
            .fold((0, 0, 0), |(x, o, v), r| (x + r.x, o + r.o, v + r.visited));

        println!("{}  x:{:4} o:{:4} / {:4}", i, x, o, v);
        */
    }

    println!("reachable boards: {}", total);
}
