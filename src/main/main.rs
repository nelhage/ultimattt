mod epsilon_greedy;
mod selfplay;
mod subprocess;
mod worker;

use ansi_term::Style;
use regex::Regex;
use serde;
use serde::Serialize;
use serde_json;
use structopt::StructOpt;
use ultimattt::game;
use ultimattt::minimax;
use ultimattt::minimax::AI;
use ultimattt::prove;

use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;
use std::str::FromStr;
use std::time::Duration;

fn cell(g: game::CellState) -> &'static str {
    match g {
        game::CellState::Empty => ".",
        game::CellState::Played(game::Player::X) => "X",
        game::CellState::Played(game::Player::O) => "O",
    }
}

fn stylefor(g: &game::Game, boards: &[usize]) -> Style {
    let style = Style::default();
    if boards
        .iter()
        .any(|b| g.board_to_play().map(|n| n == *b).unwrap_or(false))
    {
        return style.underline();
    }
    return style;
}

fn render(out: &mut dyn io::Write, g: &game::Game) -> Result<(), io::Error> {
    write!(out, "{} to play:\n", g.player())?;
    for brow in 0..3 {
        if brow != 0 {
            write!(out, "---+---+---\n")?;
        }
        for bcol in 0..3 {
            if bcol != 0 {
                write!(out, " |")?;
            }
            let ch = match g.board_state(3 * brow + bcol as usize) {
                game::BoardState::InPlay => " ",
                game::BoardState::Drawn => "#",
                game::BoardState::Won(game::Player::X) => "X",
                game::BoardState::Won(game::Player::O) => "O",
            };
            write!(out, " {}", ch)?;
        }
        write!(out, "      ")?;
        let ch = 'a' as u8 + 3 * (brow as u8);
        write!(
            out,
            "{}    {}    {}",
            ch as char,
            (ch + 1) as char,
            (ch + 2) as char,
        )?;
        write!(out, "\n")?;
    }
    writeln!(out, "")?;

    for row in 0..9 {
        if row > 0 && row % 3 == 0 {
            writeln!(out, "--------+---------+--------")?;
        }
        for col in 0..9 {
            let board = 3 * (row / 3) + col / 3;
            let sq = 3 * (row % 3) + col % 3;
            let at = g.at(board, sq);
            write!(out, "{}", stylefor(g, &[board]).paint(cell(at)))?;
            if col != 8 {
                match col % 3 {
                    0 | 1 => {
                        write!(out, "{}", stylefor(g, &[board]).paint("  "))?;
                    }
                    2 => {
                        write!(out, "{}", stylefor(g, &[board]).paint(" "))?;
                        write!(out, "|")?;
                        write!(out, "{}", stylefor(g, &[board + 1]).paint(" "))?;
                    }
                    _ => unreachable!(),
                };
            }
        }
        write!(out, "\n")?;
    }

    write!(out, "notation:\n{}\n", game::notation::render(g))?;

    Ok(())
}

pub trait Readline {
    fn read_line(&self, buf: &mut String) -> io::Result<usize>;
}

impl Readline for io::Stdin {
    fn read_line(&self, buf: &mut String) -> io::Result<usize> {
        self.read_line(buf)
    }
}

struct CLIPlayer<'a> {
    stdin: Box<dyn Readline + 'a>,
    stdout: Box<dyn io::Write + 'a>,
}

impl<'a> minimax::AI for CLIPlayer<'a> {
    fn select_move(&mut self, g: &game::Game) -> Result<game::Move, minimax::Error> {
        loop {
            write!(&mut self.stdout, "move> ").unwrap();
            self.stdout.flush().unwrap();

            let mut line = String::new();
            if let Ok(b) = self.stdin.read_line(&mut line) {
                if b == 0 {
                    return Err(minimax::Error::Quit);
                }
            } else {
                return Err(minimax::Error::Quit);
            }
            match game::notation::parse_move(line.trim_end()) {
                Ok(m) => {
                    return Ok(m);
                }
                Err(e) => {
                    writeln!(&mut self.stdout, "Bad move: {:?}", e).unwrap();
                    render(&mut self.stdout, g).unwrap();
                }
            }
        }
    }
}

fn parse_duration(arg: &str) -> Result<Duration, io::Error> {
    let pat =
        Regex::new(r"^(?:([0-9]+)h\s*)?(?:([0-9]+)m\s*)?(?:([0-9]+)s\s*)?(?:([0-9]+)ms\s*)?$")
            .unwrap();
    match pat.captures(arg) {
        None => Err(io::Error::new(
            io::ErrorKind::Other,
            format!("Unable to parse duration: '{}'", arg),
        )),
        Some(caps) => {
            let mut secs: u64 = 0;
            let mut msecs: u64 = 0;
            if let Some(h) = caps.get(1) {
                secs += 60 * 60 * h.as_str().parse::<u64>().unwrap();
            }
            if let Some(m) = caps.get(2) {
                secs += 60 * m.as_str().parse::<u64>().unwrap();
            }
            if let Some(s) = caps.get(3) {
                secs += s.as_str().parse::<u64>().unwrap();
            }
            if let Some(ms) = caps.get(4) {
                msecs += ms.as_str().parse::<u64>().unwrap();
            }
            Ok(Duration::from_millis(1000 * secs + msecs))
        }
    }
}

fn parse_bytesize(arg: &str) -> Result<bytesize::ByteSize, io::Error> {
    let pat = Regex::new(r"^([0-9]+)([KMGTPE]?i?)B?$").unwrap();
    match pat.captures(arg) {
        None => Err(io::Error::new(
            io::ErrorKind::Other,
            format!("Unable to parse bytesize: '{}'", arg),
        )),
        Some(captures) => {
            let num = captures.get(1).unwrap().as_str().parse::<u64>().unwrap();
            let unit = captures.get(2).unwrap().as_str();
            match unit {
                "" => Ok(bytesize::ByteSize::b(num)),
                "K" => Ok(bytesize::ByteSize::kb(num)),
                "Ki" => Ok(bytesize::ByteSize::kib(num)),
                "M" => Ok(bytesize::ByteSize::mb(num)),
                "Mi" => Ok(bytesize::ByteSize::mib(num)),
                "G" => Ok(bytesize::ByteSize::gb(num)),
                "Gi" => Ok(bytesize::ByteSize::gib(num)),
                "T" => Ok(bytesize::ByteSize::tb(num)),
                "Ti" => Ok(bytesize::ByteSize::tib(num)),
                _ => Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Unsupported unite: '{}'", unit),
                )),
            }
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "ultimatett", about = "Ultimate Tic Tac Toe")]
pub struct Opt {
    #[structopt(long, default_value = "1s", parse(try_from_str=parse_duration))]
    timeout: Duration,
    #[structopt(long)]
    depth: Option<i64>,
    #[structopt(long, default_value = "1G", parse(try_from_str=parse_bytesize))]
    table_mem: bytesize::ByteSize,
    #[structopt(long, default_value = "1")]
    threads: usize,
    #[structopt(long, default_value = "1")]
    debug: usize,
    #[structopt(subcommand)]
    cmd: Command,
}

#[derive(Debug, StructOpt)]
struct SelfplayParameters {
    #[structopt(short = "1", long)]
    player1: String,
    #[structopt(short = "2", long)]
    player2: String,
    #[structopt(long)]
    games: usize,
    #[structopt(long, default_value = "0.05")]
    epsilon: f64,
}

#[derive(Debug, StructOpt)]
enum Engine {
    Minimax,
    PN,
    DFPN,
    SPDFPN,
    #[allow(non_camel_case_types)]
    PN_DFPN,
}

impl FromStr for Engine {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "minimax" => Ok(Engine::Minimax),
            "pn" => Ok(Engine::PN),
            "dfpn" => Ok(Engine::DFPN),
            "spdfpn" => Ok(Engine::SPDFPN),
            "pn-dfpn" => Ok(Engine::PN_DFPN),
            _ => Err(format!("Unknown engine: `{}`", s)),
        }
    }
}

#[derive(Debug, StructOpt)]
struct AnalyzeParameters {
    #[structopt(long, default_value = "minimax")]
    engine: Engine,
    #[structopt(long)]
    max_nodes: Option<usize>,
    #[structopt(long)]
    max_work_per_job: Option<u64>,
    #[structopt(long, default_value = "0.125")]
    epsilon: f64,
    #[structopt(long)]
    dump_table: Option<String>,
    #[structopt(long)]
    load_table: Option<String>,
    #[structopt(long)]
    minimax_cutoff: Option<usize>,
    #[structopt(long)]
    write_metrics: Option<String>,
    #[structopt(long, default_value = "60s", parse(try_from_str=parse_duration))]
    dump_interval: Duration,
    #[structopt(long)]
    probe_hash: Option<u64>,
    #[structopt(long)]
    probe: Option<String>,
    #[structopt(long, default_value = "probe.csv")]
    probe_log: String,
    #[structopt(long)]
    split_threshold: Option<u64>,
    #[structopt(long)]
    queue_depth: Option<usize>,
    position: String,
}

#[derive(Debug, StructOpt)]
enum Command {
    Play {
        #[structopt(short = "x", long, default_value = "human")]
        playerx: String,
        #[structopt(short = "o", long, default_value = "minimax")]
        playero: String,
    },
    Analyze(AnalyzeParameters),
    Selfplay(SelfplayParameters),
    Worker {},
}

fn ai_config(opt: &Opt) -> minimax::Config {
    minimax::Config {
        timeout: Some(opt.timeout),
        max_depth: opt.depth,
        debug: opt.debug,
        table_bytes: Some(opt.table_mem.as_u64() as usize),
        ..Default::default()
    }
}

fn dfpn_config(opt: &Opt, analyze: &AnalyzeParameters, pos: &game::Game) -> prove::dfpn::Config {
    let probe_hash = match (analyze.probe_hash, analyze.probe.as_ref()) {
        (None, None) => None,
        (Some(p), None) => Some(p),
        (None, Some(moves)) => Some(if moves.len() == 0 {
            pos.zobrist()
        } else {
            moves
                .split(" ")
                .fold(pos.clone(), |pos, mv| {
                    pos.make_move(game::notation::parse_move(mv).expect("parse_move"))
                        .expect("make_move")
                })
                .zobrist()
        }),
        (Some(_), Some(_)) => panic!("--probe-hash and --probe are incompatible"),
    };
    let mut cfg = prove::dfpn::Config {
        threads: opt.threads,
        table_size: opt.table_mem.as_u64() as usize,
        timeout: Some(opt.timeout),
        debug: opt.debug,
        epsilon: analyze.epsilon,
        dump_table: analyze.dump_table.clone(),
        load_table: analyze.load_table.clone(),
        dump_interval: analyze.dump_interval.clone(),
        probe_log: analyze.probe_log.clone(),
        probe_hash: probe_hash,
        ..Default::default()
    };
    if let Some(m) = analyze.max_work_per_job {
        cfg.max_work_per_job = m;
    }
    if let Some(c) = analyze.minimax_cutoff {
        cfg.minimax_cutoff = c;
    };
    cfg
}

fn make_ai(opt: &Opt) -> minimax::Minimax {
    minimax::Minimax::with_config(&ai_config(opt))
}

fn parse_player<'a>(
    opt: &Opt,
    player: game::Player,
    spec: &str,
) -> Result<Box<dyn minimax::AI + 'a>, std::io::Error> {
    if spec == "human" {
        Ok(Box::new(CLIPlayer {
            stdin: Box::new(io::stdin()),
            stdout: Box::new(io::stdout()),
        }))
    } else if spec.starts_with("minimax") {
        Ok(Box::new(make_ai(&opt)))
    } else if spec.starts_with("subprocess@") {
        let tail = spec.splitn(2, '@').nth(1).unwrap();
        let args = tail
            .split_whitespace()
            .map(|s| s.to_owned())
            .collect::<Vec<String>>();
        Ok(Box::new(subprocess::Player::new(
            args,
            player,
            &ai_config(opt),
        )?))
    } else {
        Err(io::Error::new(
            io::ErrorKind::Other,
            format!("bad player spec: {}", spec),
        ))
    }
}

fn build_selfplay_player<'a>(
    player: game::Player,
    config: &minimax::Config,
    cmd: &str,
    epsilon: f64,
) -> Result<Box<dyn minimax::AI + 'a>, io::Error> {
    let args = cmd
        .split_whitespace()
        .map(|s| s.to_owned())
        .collect::<Vec<String>>();
    let player = subprocess::Player::new(args, player, &config)?;
    if epsilon != 0.0 {
        Ok(Box::new(epsilon_greedy::Player::new(
            Box::new(player),
            epsilon,
        )))
    } else {
        Ok(Box::new(player))
    }
}

#[derive(Serialize)]
struct Metrics<T: Serialize> {
    duration_ms: u128,
    #[serde(flatten)]
    stats: T,
}

fn main() -> Result<(), std::io::Error> {
    let opt = Opt::from_args();
    let mut stdout = io::stdout();
    match opt.cmd {
        Command::Play {
            ref playerx,
            ref playero,
            ..
        } => {
            let mut g = game::Game::new();
            let mut player_x = parse_player(&opt, game::Player::X, playerx)?;
            let mut player_o = parse_player(&opt, game::Player::O, playero)?;
            loop {
                render(&mut stdout, &g)?;
                let m = match g.player() {
                    game::Player::X => player_x.select_move(&g),
                    game::Player::O => player_o.select_move(&g),
                };
                if let Err(e) = m {
                    match e {
                        minimax::Error::Other(msg) => panic!(msg),
                        minimax::Error::Quit => break,
                    }
                }
                let m = m.unwrap();

                match g.make_move(m) {
                    Ok(gg) => {
                        g = gg;
                    }
                    Err(e) => {
                        println!("bad move: {:?}", e);
                    }
                };
                match g.game_state() {
                    game::BoardState::InPlay => (),
                    _ => break,
                }
            }
        }
        Command::Analyze(ref analyze) => {
            let game = match game::notation::parse(&analyze.position) {
                Ok(g) => g,
                Err(e) => {
                    println!("Parsing position: {:?}", e);
                    exit(1)
                }
            };
            match analyze.engine {
                Engine::PN => {
                    let result = prove::pn::Prover::prove(
                        &prove::pn::Config {
                            debug: opt.debug,
                            timeout: Some(opt.timeout),
                            max_nodes: analyze.max_nodes,
                            ..Default::default()
                        },
                        &game,
                    );
                    println!(
                        "result={:?} time={}.{:03}s pn={} dpn={} searched={}",
                        result.result,
                        result.duration.as_secs(),
                        result.duration.subsec_millis(),
                        result.proof,
                        result.disproof,
                        result.allocated,
                    );
                }
                Engine::DFPN | Engine::SPDFPN => {
                    let mut dfpn_cfg = dfpn_config(&opt, &analyze, &game);
                    if let Engine::DFPN = analyze.engine {
                        dfpn_cfg.threads = 0;
                    };

                    let result = prove::dfpn::DFPN::prove(&dfpn_cfg, &game);
                    println!(
                    "result={:?} time={}.{:03}s pn={} dpn={} mid={} try={} jobs={} tthit={}/{} ({:.1}%) ttstore={} minimax={}/{}",
                    result.value,
                    result.duration.as_secs(),
                    result.duration.subsec_millis(),
                    result.bounds.phi,
                    result.bounds.delta,
                    result.stats.mid,
                    result.stats.try_calls,
                    result.stats.jobs,
                    result.stats.tt.hits,
                    result.stats.tt.lookups,
                    100.0 * (result.stats.tt.hits as f64 / result.stats.tt.lookups as f64),
                    result.stats.tt.stores,
                    result.stats.minimax_solve,
                    result.stats.minimax,
                );
                    println!(
                        "  perf: mid/ms={:.2} job/ms={:.2}",
                        (result.stats.mid as f64) / (result.duration.as_millis() as f64),
                        (result.stats.jobs as f64) / (result.duration.as_millis() as f64),
                    );
                    let mut pv = String::new();
                    for m in result.pv.iter() {
                        write!(&mut pv as &mut dyn std::fmt::Write, "{} ", m).unwrap();
                    }
                    println!(" pv={}", pv.trim_end());
                }
                Engine::PN_DFPN => {
                    let cfg = {
                        let dfpn_cfg = prove::dfpn::Config {
                            debug: opt.debug.saturating_sub(10),
                            ..dfpn_config(&opt, &analyze, &game)
                        };
                        let mut cfg = prove::pn_dfpn::Config {
                            debug: opt.debug,
                            timeout: Some(opt.timeout),
                            max_nodes: analyze.max_nodes,
                            dfpn: dfpn_cfg,
                            ..Default::default()
                        };
                        if let Some(st) = analyze.split_threshold {
                            cfg.split_threshold = st;
                        }
                        if let Some(q) = analyze.queue_depth {
                            cfg.queue_depth = q;
                        }
                        cfg
                    };
                    let result = prove::pn_dfpn::Prover::prove(&cfg, &game);
                    println!(
                        "result={:?} time={}.{:03}s pn={} dpn={} jobs={} recv={} searched={}",
                        result.result,
                        result.duration.as_secs(),
                        result.duration.subsec_millis(),
                        result.proof,
                        result.disproof,
                        result.stats.jobs,
                        result.stats.recv,
                        result.stats.allocated,
                    );
                    println!(
                        "  mid={} try={} jobs={} tthit={}/{} ({:.1}%) ttstore={} minimax={}/{}",
                        result.stats.mid.mid,
                        result.stats.mid.try_calls,
                        result.stats.mid.jobs,
                        result.stats.mid.tt.hits,
                        result.stats.mid.tt.lookups,
                        100.0
                            * (result.stats.mid.tt.hits as f64
                                / result.stats.mid.tt.lookups as f64),
                        result.stats.mid.tt.stores,
                        result.stats.mid.minimax_solve,
                        result.stats.mid.minimax,
                    );
                    if cfg.debug > 0 {
                        println!(
                            " us/job   mean={:.1} p10={} p50={} p90={} p99={}",
                            result.stats.worker.us_per_job.mean(),
                            result.stats.worker.us_per_job.value_at_quantile(0.1),
                            result.stats.worker.us_per_job.value_at_quantile(0.5),
                            result.stats.worker.us_per_job.value_at_quantile(0.9),
                            result.stats.worker.us_per_job.value_at_quantile(0.99),
                        );
                        println!(
                            " work/job mean={:.1} p10={} p50={} p90={} p99={}",
                            result.stats.worker.work_per_job.mean(),
                            result.stats.worker.work_per_job.value_at_quantile(0.1),
                            result.stats.worker.work_per_job.value_at_quantile(0.5),
                            result.stats.worker.work_per_job.value_at_quantile(0.9),
                            result.stats.worker.work_per_job.value_at_quantile(0.99),
                        );
                    }
                    if let Some(ref p) = analyze.write_metrics {
                        let metrics = Metrics {
                            duration_ms: result.duration.as_millis(),
                            stats: result.stats.clone(),
                        };
                        let mut f = fs::OpenOptions::new()
                            .read(false)
                            .append(true)
                            .write(true)
                            .truncate(false)
                            .create(true)
                            .open(p)
                            .expect("write_metrics");
                        serde_json::to_writer(&mut f, &metrics)?;
                        f.write_all(b"\n")?;
                    }
                }
                Engine::Minimax => {
                    let mut ai = make_ai(&opt);
                    let m = ai.select_move(&game).map_err(|e| {
                        io::Error::new(io::ErrorKind::Other, format!("analyzing: {:?}", e))
                    })?;
                    println!("move={}", game::notation::render_move(m));
                }
            };
        }
        Command::Worker { .. } => {
            let mut stdin = io::stdin();
            let mut stdout = io::stdout();

            let mut worker = worker::Worker::new(&mut stdin, &mut stdout, &opt);
            worker.run()?;
        }
        Command::Selfplay(ref params) => {
            let config = ai_config(&opt);
            let builder1 = |p| build_selfplay_player(p, &config, &params.player1, params.epsilon);
            let builder2 = |p| build_selfplay_player(p, &config, &params.player2, params.epsilon);
            let mut engine = selfplay::Executor::new(builder1, builder2);
            let res = engine.execute(params.games)?;
            println!(
                "self-play games={} p1={} p2={} draw={}",
                res.games,
                res.player1_wins(),
                res.player2_wins(),
                res.draws(),
            );
            let p1_x = res.p1_x_results();
            let p1_o = res.p1_o_results();
            println!(
                "  p1@X: games={} p1={} p2={} draw={}",
                p1_x.0 + p1_x.1 + p1_x.2,
                p1_x.0,
                p1_x.1,
                p1_x.2,
            );
            println!(
                "  p1@O: games={} p1={} p2={} draw={}",
                p1_o.0 + p1_o.1 + p1_o.2,
                p1_o.0,
                p1_o.1,
                p1_o.2,
            );
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_duration() {
        let tests: &[(&'static str, Option<Duration>)] = &[
            ("5s", Some(Duration::from_secs(5))),
            ("5m", Some(Duration::from_secs(60 * 5))),
            ("5m 8s", Some(Duration::from_secs(60 * 5 + 8))),
            (
                "2h3s 1ms",
                Some(Duration::from_secs(60 * 60 * 2 + 3) + Duration::from_millis(1)),
            ),
            ("5sec", None),
            ("5d", None),
            ("4h4s3m", None),
        ];
        for tc in tests {
            let got = parse_duration(tc.0);
            match (got, tc.1) {
                (Err(..), None) => (),
                (Ok(s), None) => {
                    panic!("parse({}): got {:?}, expected error", tc.0, s);
                }
                (Err(e), Some(d)) => {
                    panic!("parse({}): got err({:?}), expected {:?}", tc.0, e, d);
                }
                (Ok(g), Some(d)) => {
                    if g != d {
                        panic!("parse({}): got {:?}, expected {:?}", tc.0, g, d);
                    }
                }
            }
        }
    }
}
