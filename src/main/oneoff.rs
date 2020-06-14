#![allow(dead_code)]
use bit_set::BitSet;
use packed_simd::u16x8;
// use std::time::Instant;

mod symmetry;

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
    fn new() -> Self {
        Game { x: 0, o: 0 }
    }

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
        (self.x as usize) | ((self.o as usize) << 9)
    }

    fn from_bits(bits: usize) -> Self {
        Self {
            x: (bits & 0x1ff) as u16,
            o: ((bits >> 9) & 0x1ff) as u16,
        }
    }

    fn at(self, i: usize) -> char {
        let bit = 1_u16 << i;
        match (self.x & bit != 0, self.o & bit != 0) {
            (true, false) => 'X',
            (false, true) => 'O',
            (false, false) => '.',
            (true, true) => '#',
        }
    }

    fn empty(self, idx: usize) -> bool {
        (self.o | self.x) & (1_u16 << idx) == 0
    }

    fn count(self) -> usize {
        (self.o | self.x).count_ones() as usize
    }
    fn count_x(self) -> usize {
        (self.x & !self.o).count_ones() as usize
    }
    fn count_o(self) -> usize {
        (self.o & !self.x).count_ones() as usize
    }

    fn make_move(self, idx: usize, who: u16) -> Game {
        Game {
            x: self.x | ((who & 1) << idx),
            o: self.o | (((who & 2) >> 1) << idx),
        }
    }
    fn apply(self, perm: &symmetry::Permutation) -> Game {
        Game {
            x: perm.apply(self.x),
            o: perm.apply(self.o),
        }
    }

    fn to_str(self) -> String {
        format!(
            "{} {} {}\n{} {} {}\n{} {} {}\n",
            self.at(0),
            self.at(1),
            self.at(2),
            self.at(3),
            self.at(4),
            self.at(5),
            self.at(6),
            self.at(7),
            self.at(8),
        )
    }
}

#[derive(Default, Clone, Copy)]
struct Counts {
    visited: usize,
    x: usize,
    o: usize,
}

struct Brute {
    draws: bool,
    do_symmetry: bool,
    total: usize,
    stats: [[Counts; 8]; 8],
    seen: bit_set::BitSet,
}

impl Brute {
    fn search(&mut self, g: Game) {
        if self.do_symmetry {
            if self.seen.contains(g.as_index()) {
                return;
            }
            for perm in symmetry::PERM_TABLES.iter() {
                self.seen.insert(g.apply(&perm).as_index());
                /*
                if self.seen.contains(g.permute(&perm).as_index()) {
                    return;
                }*/
            }
        } else {
            if self.seen.contains(g.as_index()) {
                return;
            }
            self.seen.insert(g.as_index());
        }

        self.total += 1;
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
                self.search(g.make_move(i, 0x1));
                self.search(g.make_move(i, 0x2));
                if self.draws {
                    self.search(g.make_move(i, 0x3));
                }
            }
        }
    }
}

fn main() {
    let mut brute = Brute {
        do_symmetry: true,
        draws: true,
        stats: Default::default(),
        seen: BitSet::new(),
        total: 0,
    };
    brute.search(Game { x: 0, o: 0 });

    println!(
        "reachable boards with draws(modulo symmetry): {} total={}",
        brute.total,
        brute.seen.len()
    );

    for (x, row) in brute.stats.iter().enumerate() {
        print!("x={}", x);
        for (_o, e) in row.iter().enumerate() {
            print!("  {:3}/{:-3}/{:3}|", e.x, e.o, e.visited);
        }
        println!();
    }

    println!();
    println!();

    let mut brute_inner = Brute {
        do_symmetry: true,
        draws: false,
        stats: Default::default(),
        seen: BitSet::new(),
        total: 0,
    };
    brute_inner.search(Game { x: 0, o: 0 });

    println!(
        "reachable boards w/o draws (modulo symmetry): {} total={}",
        brute_inner.total,
        brute_inner.seen.len()
    );

    for (x, row) in brute_inner.stats.iter().enumerate() {
        print!("x={}", x);
        for (_o, e) in row.iter().enumerate() {
            print!("  {:3}/{:-3}/{:3}|", e.x, e.o, e.visited);
        }
        println!();
    }
}
