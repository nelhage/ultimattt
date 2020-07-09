# Ultimate Tic Tac Toe

This repository contains an implementation of [Ultimate Tic Tac Toe][ultimate], as well as a minimax AI engine and a high-performance solver for solving positions.

This project has aspirations of solving the game of Ultimate Tic Tac Toe, but we are currently some distance away. You can [read my full writeup about the project and the pieces of the solver][writeup]. This README attempts to document how to build and use the code here.

# Building

This project requires Rust nightly. I recommend installing Rust [via rustup][rustup] if you don't already have it installed. Release builds are recommended if you want to use the solver or minimax AI, since both are very performance sensitive.

# Notation

I am not aware of a standard notational convention for Ultimate Tic Tac Toe, so I've devised a somewhat ad-hoc convention for this engine. I denote squares in either a local or global board using the letters `a` through `i`, like so:

```
a b c
d e f
g h i
```

Moves are denoted by a two-letter pair, naming first which local board is being played in, and then the square within that board; for instance, `ae` denotes the center square in the top-left local game.

Positions are rendered for compact output and input like so:

```
O;@........;X.OO...../X..X.O.O./X.X...O.O/.X.OXO.../O.O.X..../.XX....O./......X.O/.O.X.X.../.O.....XX
```

Which is to say, as three semicolon-separated components: The player who is to play, the state of the global board, and the state of each local board, delimited by `/`. On the global board, an `@` indicates the board the next player must play in.

# Running

The crate builds a single binary, `ultimattt`, which contains several subcommands. Run it via `cargo run --release -- <arguments>` or out of `target/release/ultimattt` after a `cargo build --release`.

## ultimattt pp [POSITION]

`pp` accepts as an argument a board in the notation above, and prints it to the terminal in a more human-readable format.

## ultimattt play [ARGS]

`play` launches a CLI driver to play a game on the command line. You can use the `-x` and `-o` options to select human or a minimax AI for each player, and use [the common minimax options](#common-minimax-options) to control the minimax AI.

## ultimattt analyze [ARGS] POSITON

`analyze` analyzes a position using one of several available analysis engines (via `--engine`). The available engines are:

- `--engine=minimax` to use the minimax AI. See [the common minimax options](#common-minimax-options) for flags to control this engine.
- `--engine=pn` uses a vanilla [proof number search tree][pn]. This algorithm is fast but memory-intensive.
- `--engine=dfpn` uses [depth-first proof number search][dfpn]. This is somewhat slower than `pn` but uses a fixed memory budget (determined by the transposition table memory) and contains some additional optimizations.
- `--engine=pn-dfpn` uses my [hybrid parallel PN/DFPN searcher][pn-dfpn]. This is the best proof engine available in this program.
- `--engine=endgame` dumps information about [the positional analysis][positional] used by the DFPN/PN-DFPN solvers to prune the search tree.

### Options

`analyze` exposes various options to control the solvers. Most of these apply to a subset of the engines.

 - `--dump-interval=T` Dump the transposition table to path named by `--dump-table` every period of T.
 - `--dump-table=PATH` Path to dump the transposition table to checkpoint progress. Only applies to `dfpn` and `pn-dfpn`.
 - `--epsilon=ε` ε value for `1+ε` searching in DFPN and PN-DFPN.
 - `--limit=T` Limit the search time. Stop after this time limit and print results so far.
 - `--load-table=PATH` Reload a table stored by `--dump-table`.
 - `--max-mem=MEM` Limit the amount of memory used by the PN search tree for `pn` and `pn-dfpn`.
 - `--split-threshold` The work limit per node in the top-level search tree for `pn-dfpn`.
 - `--table-mem=MEM` Select the size of the transposition table for `dfpn` and `pn-dfpn`. Larger tends to perform better.
 - `--threads` The number of worker threads to use for `pn-dfpn`'s parallel search. Actually uses N+1 threads in total.
 - `--variation=LINE` Apply a sequence of moves on top of the provided position before analyzing.
 - `--write-metric=PATH` Dump metrics about the search to a JSON file on exit.

### Common minimax options

`ultimattt play` and `ultimattt analyze --engine=minimax` accept an overlapping set of flags to control the minimax AI engine. You can pass:

- `--limit=T` to limit the search time to the specified budget
- `--table-mem=MEM` to control how much memory is allocated to the transposition table.
- `--depth=D` to limit the depth of the search. By default it is unlimited and is limited only by the time budget from `--limit`.

[pn]: https://www.minimax.dev/docs/ultimate/pn-search/
[dfpn]: https://www.minimax.dev/docs/ultimate/pn-search/dfpn/
[pn-dfpn]: https://www.minimax.dev/docs/ultimate/pn-dfpn/
[positional]: https://www.minimax.dev/docs/ultimate/pruning/
[ultimate]: https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe
[writeup]: https://www.minimax.dev/docs/ultimate/
[rustup]: https://rustup.rs/
