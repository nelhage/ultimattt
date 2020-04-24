mod display;
pub mod notation;

use std::hash::{Hash, Hasher};
use std::vec::Vec;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Player {
    X,
    O,
}

impl Player {
    pub fn other(&self) -> Player {
        match self {
            Player::X => Player::O,
            Player::O => Player::X,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Player::X => "X",
            Player::O => "O",
        }
    }

    pub fn as_bit(&self) -> usize {
        match self {
            Player::X => 0,
            Player::O => 1,
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum CellState {
    Empty,
    Played(Player),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BoardState {
    InPlay,
    Drawn,
    Won(Player),
}

impl BoardState {
    pub fn terminal(&self) -> bool {
        match self {
            BoardState::Drawn | BoardState::Won(_) => true,
            _ => false,
        }
    }
}

pub(in crate) const WIN_PATTERNS: [[usize; 3]; 8] = [
    // rows
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    // columns
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    // diagonals
    [0, 4, 8],
    [6, 4, 2],
];

pub(in crate) const WIN_MASKS: &[u32] = &[0x7, 0x38, 0x1c0, 0x49, 0x92, 0x124, 0x111, 0x54];
const BOARD_MASK: u32 = 0x1ff;

trait HasOwner {
    fn player(&self) -> Option<Player>;
    fn empty(&self) -> bool;
}

impl HasOwner for CellState {
    fn player(&self) -> Option<Player> {
        match self {
            CellState::Empty => None,
            CellState::Played(p) => Some(*p),
        }
    }

    fn empty(&self) -> bool {
        match self {
            CellState::Empty => true,
            _ => false,
        }
    }
}

impl HasOwner for BoardState {
    fn player(&self) -> Option<Player> {
        match self {
            BoardState::Won(p) => Some(*p),
            _ => None,
        }
    }
    fn empty(&self) -> bool {
        match self {
            BoardState::InPlay => true,
            _ => false,
        }
    }
}

fn check_winner<T: HasOwner>(board: &[T; 9], who: Player) -> BoardState {
    if WIN_PATTERNS.iter().any(|&pat| {
        pat.iter().all(|i| match board[*i].player() {
            Some(p) => p == who,
            _ => false,
        })
    }) {
        return BoardState::Won(who);
    }

    if board.iter().any(|e| e.empty()) {
        BoardState::InPlay
    } else {
        BoardState::Drawn
    }
}

#[derive(Clone, Debug)]
pub struct Subboard([CellState; 9]);

#[derive(Clone, Debug)]
pub struct Unpacked {
    pub next_player: Player,
    pub next_board: Option<u8>,
    pub boards: [Subboard; 9],
    pub game_states: [BoardState; 9],
    pub overall_state: BoardState,
}

impl Default for Unpacked {
    fn default() -> Self {
        let board = Subboard([CellState::Empty; 9]);
        Self {
            next_player: Player::X,
            next_board: None,
            boards: [
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
                board.clone(),
            ],
            game_states: [BoardState::InPlay; 9],
            overall_state: BoardState::InPlay,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Row {
    // These are each a packed [u9; 3] containing a bitmask for the
    // respective player's states. The low bits store index 0.
    x: u32,
    o: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate) struct Subboards {
    rows: [Row; 3],
}

impl Subboards {
    fn at(&self, board: usize, cell: usize) -> CellState {
        let row = &self.rows[board / 3];
        let idx = (board % 3) * 9 + cell;
        let xbit = (row.x >> idx) & 1 == 1;
        let obit = (row.o >> idx) & 1 == 1;
        if xbit {
            return CellState::Played(Player::X);
        }
        if obit {
            return CellState::Played(Player::O);
        }
        return CellState::Empty;
    }

    fn set(&mut self, board: usize, cell: usize, who: Player) {
        let mut row = &mut self.rows[board / 3];
        let idx = (board % 3) * 9 + cell;
        let bit = 1 << idx;
        match who {
            Player::X => {
                row.x |= bit;
            }
            Player::O => {
                row.o |= bit;
            }
        }
    }

    fn full(&self, board: usize) -> bool {
        let row = &self.rows[board / 3];
        let mask = BOARD_MASK << 9 * (board % 3);
        (row.o | row.x) & mask == mask
    }

    pub(in crate) fn xbits(&self, board: usize) -> u32 {
        let row = &self.rows[board / 3];
        (row.x >> (9 * (board % 3))) & BOARD_MASK
    }

    pub(in crate) fn obits(&self, board: usize) -> u32 {
        let row = &self.rows[board / 3];
        (row.o >> (9 * (board % 3))) & BOARD_MASK
    }

    fn mask(&self, board: usize) -> u32 {
        let row = &self.rows[board / 3];
        (row.o | row.x) >> (9 * (board % 3)) & BOARD_MASK
    }

    fn check_winner(&self, board: usize, player: Player) -> BoardState {
        let row = &self.rows[board / 3];
        let shift = 9 * (board % 3);
        let mask = match player {
            Player::X => row.x >> shift,
            Player::O => row.o >> shift,
        };
        for win in WIN_MASKS {
            if mask & win == *win {
                return BoardState::Won(player);
            }
        }
        if ((row.x | row.o) >> shift) & BOARD_MASK == BOARD_MASK {
            BoardState::Drawn
        } else {
            BoardState::InPlay
        }
    }
}

impl Default for Subboards {
    fn default() -> Self {
        Subboards {
            rows: [Row { x: 0, o: 0 }, Row { x: 0, o: 0 }, Row { x: 0, o: 0 }],
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate) struct GameStates {
    // (x: u9, o: u9, drawn: u9), LSB first
    bits: u32,
}

impl Default for GameStates {
    fn default() -> Self {
        GameStates { bits: 0 }
    }
}

impl GameStates {
    pub(in crate) fn xbits(&self) -> u32 {
        self.bits & BOARD_MASK
    }
    pub(in crate) fn obits(&self) -> u32 {
        (self.bits >> 9) & BOARD_MASK
    }
    pub(in crate) fn drawbits(&self) -> u32 {
        (self.bits >> 18) & BOARD_MASK
    }
    pub(in crate) fn donebits(&self) -> u32 {
        (self.bits | (self.bits >> 9) | (self.bits >> 18)) & BOARD_MASK
    }
    pub(in crate) fn playerbits(&self, player: Player) -> u32 {
        match player {
            Player::X => self.xbits(),
            Player::O => self.obits(),
        }
    }
    pub(in crate) fn in_play(&self, board: usize) -> bool {
        (self.donebits() & 1 << board) == 0
    }

    fn check_winner(&self, player: Player) -> BoardState {
        let mask = self.playerbits(player);
        for win in WIN_MASKS {
            if mask & win == *win {
                return BoardState::Won(player);
            }
        }
        if self.donebits() == BOARD_MASK {
            BoardState::Drawn
        } else {
            BoardState::InPlay
        }
    }

    fn at(&self, board: usize) -> BoardState {
        let bit = 1 << board;
        if self.xbits() & bit != 0 {
            BoardState::Won(Player::X)
        } else if self.obits() & bit != 0 {
            BoardState::Won(Player::O)
        } else if self.drawbits() & bit != 0 {
            BoardState::Drawn
        } else {
            BoardState::InPlay
        }
    }

    fn set(&mut self, board: usize, state: BoardState) {
        let mut i = board;
        match state {
            BoardState::Drawn => {
                i += 18;
            }
            BoardState::InPlay => {
                return;
            }
            BoardState::Won(Player::X) => {}
            BoardState::Won(Player::O) => {
                i += 9;
            }
        };
        self.bits |= 1 << i;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Game {
    pub(in crate) next_player: Player,
    pub(in crate) next_board: Option<u8>,
    pub(in crate) boards: Subboards,
    pub(in crate) game_states: GameStates,
    pub(in crate) overall_state: BoardState,
    hash: u64,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct Move {
    bits: u8,
}

impl Move {
    pub fn from_coords(board: usize, square: usize) -> Self {
        return Self {
            bits: (board << 4 | square) as u8,
        };
    }

    pub fn board(self) -> usize {
        (self.bits >> 4) as usize
    }

    pub fn square(self) -> usize {
        (self.bits & 0xf) as usize
    }

    pub fn none() -> Self {
        Move { bits: 0xff }
    }

    pub fn is_none(self) -> bool {
        self.bits == 0xff
    }

    pub fn is_some(self) -> bool {
        !self.is_none()
    }

    pub fn bits(self) -> u8 {
        self.bits
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum MoveError {
    WrongBoard,
    WonBoard,
    OutOfBounds,
    NotEmpty,
    GameOver,
}

struct Zobrist {
    // squares[board][cell][player.as_bit()]
    squares: [[[u64; 2]; 9]; 9],
    // boards[board] = [tie, X, O]
    boards: [[u64; 3]; 9],
    next_board: [u64; 9],
}

impl Zobrist {
    fn for_board(&self, board: usize, state: BoardState) -> u64 {
        match state {
            BoardState::InPlay => 0,
            BoardState::Drawn => self.boards[board][0],
            BoardState::Won(Player::X) => self.boards[board][1],
            BoardState::Won(Player::O) => self.boards[board][2],
        }
    }
}

static ZOBRIST_TABLES: Zobrist = Zobrist {
    squares: [
        [
            [3903901466010275279, 17629543429219029125],
            [6560537333460717463, 7986740484246722560],
            [13448444757883081222, 8862075095357973495],
            [627530935268485925, 13618924972420855844],
            [5658841495040586966, 10588138687755045676],
            [13375152968863883301, 11412907888164986633],
            [3720144703103354770, 754926789191194911],
            [10153693656287081050, 7904381750307070847],
            [5989228250479128887, 13415111613617292659],
        ],
        [
            [12059086116899244332, 7451163809502394975],
            [14996391777122309193, 10143908630119605033],
            [10754802737566917926, 1088440516914160163],
            [8386103541441791909, 12542817629116669989],
            [238424499285833975, 9653130381207852254],
            [9405888958724483071, 17126726561198328420],
            [8179610268055778624, 6553341512026093135],
            [15560361405764370159, 16746408262836215598],
            [2644246315876859064, 2061237395763558784],
        ],
        [
            [16124045022113393060, 14256613376501550053],
            [14021379837533551897, 6802895348214381347],
            [136544599174301899, 9168882058527334167],
            [3011668097940964029, 10773388103513099196],
            [4335971041406852442, 13718617498506983821],
            [9218116851555331607, 10002078858339948169],
            [3740883737439103451, 2027739035571163026],
            [4671695329495669244, 10164495824027210026],
            [2930388333517988396, 11791177830834100646],
        ],
        [
            [12195637642012800609, 4818057183511173711],
            [5746580025108558176, 8593898498462459111],
            [6915933671817634910, 500010199411656409],
            [17160765776457446381, 17499166974477630742],
            [7531030024674436671, 14444020855268728990],
            [14420961019011337767, 9953309611959569954],
            [12755489774125999224, 13386131398488311337],
            [10572301849303105890, 14245356919393356844],
            [7749810252646487439, 405107600915105234],
        ],
        [
            [4673941769004790044, 9912376699992536506],
            [13772861075326472726, 1424635966634839591],
            [10448224672211504257, 10375061792833773117],
            [810972899958790168, 297139134030571911],
            [3433409098337369845, 4634971037386423006],
            [12850522352033757384, 6596947357672707500],
            [13778958088203008837, 14423899773881843734],
            [8812853012364468235, 14203791236973137482],
            [4374649817425260023, 2753676664979010312],
        ],
        [
            [986597567356751249, 15570697413009087723],
            [15530147278100853657, 17421029532736007804],
            [2527105967667774100, 11286137556684937497],
            [304139287659296867, 7540826990568408152],
            [10166453674469440684, 6099008868128817969],
            [6622564022921803344, 9408288026298252096],
            [8349288625820524788, 2373305270142202147],
            [17564047435738048593, 234641463252374735],
            [15764970752357614869, 11458980784115948641],
        ],
        [
            [12818556854687771997, 156920060851728499],
            [18274899741850289430, 9654313182984924307],
            [12255631505909503829, 865491575069738653],
            [17256558342343098752, 17955956082692613109],
            [578210944565411855, 3600222308699921328],
            [3031820988293496544, 302500152432294006],
            [2747199606362250167, 9452470642626238316],
            [7808060539223555787, 15468571447339663953],
            [11158004304530322043, 16263219473716500958],
        ],
        [
            [17244338066467202728, 9053834545123193093],
            [11453274589915392600, 6495049422195869991],
            [6318660622973812624, 7950284959269046255],
            [1293341356545473800, 6354918147291769233],
            [12027263893405586912, 2827004469857518461],
            [2510899281429553955, 5677839733827651068],
            [15477038650223642817, 3714445228525652981],
            [14711424822796252471, 645043225026648688],
            [14451820229844848118, 16433456985459467283],
        ],
        [
            [10734409564733582363, 17888608904115112824],
            [14963165677727213545, 13735647310550505811],
            [10977874651871842400, 12388199409902683965],
            [18402500573870520482, 17645806566650012533],
            [7617244385618196477, 2403187083095025443],
            [15534413895419439910, 15322806809198407270],
            [6998107393372259430, 17484518793461397802],
            [1805771003956062686, 15508142961429924035],
            [14983629085325617298, 13988792144298142757],
        ],
    ],
    boards: [
        [
            10569293916965141230,
            11160423188805547020,
            4152050228531721776,
        ],
        [
            9445060042156387674,
            17343938531420928522,
            4567518214753125608,
        ],
        [
            10044836178330282261,
            6985788723661997541,
            2957325009523696543,
        ],
        [
            4262220721745283853,
            9629117720159326359,
            14144349062870192621,
        ],
        [
            15701379455324056445,
            13805334679043656525,
            2244893216642584252,
        ],
        [
            14647228938910723007,
            1431351858910568268,
            1852814477193620178,
        ],
        [
            10516372127856826326,
            12162076319882157197,
            4528292767345948788,
        ],
        [
            8820780412613772490,
            11456186954002538683,
            7988953696186528238,
        ],
        [
            11349973509132415302,
            8414190152471347161,
            14547444142552861968,
        ],
    ],
    next_board: [
        16798470787979032810,
        13172679987797492821,
        8295043729246619448,
        471941106522091992,
        16298057341796429847,
        7135075252472040655,
        9733518850052539775,
        5430438456607159556,
        7693123149093558032,
    ],
};

impl Game {
    pub fn new() -> Game {
        Game {
            next_player: Player::X,
            next_board: None,
            boards: Default::default(),
            game_states: Default::default(),
            overall_state: BoardState::InPlay,
            hash: 0,
        }
    }

    pub fn pack(bits: &Unpacked) -> Game {
        let mut boards: Subboards = Default::default();
        let mut hash: u64 = 0;
        for row in 0..3 {
            let mut x: u32 = 0;
            let mut o: u32 = 0;
            for col in 0..3 {
                let bnum = 3 * row + col;
                let board = &bits.boards[bnum];
                for (i, cell) in board.0.iter().enumerate() {
                    let idx: u32 = 1 << (9 * col + i);
                    match cell {
                        CellState::Played(Player::X) => {
                            x |= idx;
                            hash ^= ZOBRIST_TABLES.squares[bnum][i][Player::X.as_bit()];
                        }
                        CellState::Played(Player::O) => {
                            o |= idx;
                            hash ^= ZOBRIST_TABLES.squares[bnum][i][Player::O.as_bit()];
                        }
                        CellState::Empty => (),
                    }
                }
            }
            boards.rows[row].x = x;
            boards.rows[row].o = o;
        }
        let mut states: GameStates = Default::default();
        for i in 0..9 {
            states.set(i, bits.game_states[i]);
            hash ^= ZOBRIST_TABLES.for_board(i, bits.game_states[i]);
        }
        Game {
            next_player: bits.next_player,
            next_board: bits.next_board,
            boards: boards,
            game_states: states,
            overall_state: bits.overall_state,
            hash: hash,
        }
    }

    pub fn make_move(&self, m: Move) -> Result<Game, MoveError> {
        let mut out = self.clone();
        out.inplace_move(m).map(|_| out)
    }

    pub fn inplace_move(&mut self, m: Move) -> Result<(), MoveError> {
        if m.board() >= 9 || m.square() >= 9 {
            return Err(MoveError::OutOfBounds);
        }
        match self.overall_state {
            BoardState::InPlay => (),
            _ => return Err(MoveError::GameOver),
        }
        if !self.game_states.in_play(m.board()) {
            return Err(MoveError::WonBoard);
        }
        if let Some(b) = self.next_board {
            if b != m.board() as u8 {
                return Err(MoveError::WrongBoard);
            }
        }
        if self.boards.at(m.board(), m.square()) != CellState::Empty {
            return Err(MoveError::NotEmpty);
        }

        self.boards.set(m.board(), m.square(), self.next_player);
        self.hash ^= ZOBRIST_TABLES.squares[m.board()][m.square()][self.next_player.as_bit()];
        let board_state = self.boards.check_winner(m.board(), self.next_player);
        self.game_states.set(m.board(), board_state);
        self.hash ^= ZOBRIST_TABLES.for_board(m.board(), board_state);
        if self.game_states.in_play(m.square()) {
            self.next_board = Some(m.square() as u8);
        } else {
            self.next_board = None;
        }
        self.overall_state = self.game_states.check_winner(self.next_player);
        self.next_player = self.next_player.other();
        return Ok(());
    }

    pub fn recalc_winner(&mut self) {
        self.overall_state = self.game_states.check_winner(self.next_player.other());
    }

    pub fn all_moves<'a>(&'a self) -> impl Iterator<Item = Move> + 'a {
        MoveIterator::from_game(&self)
    }

    pub fn game_state(&self) -> BoardState {
        self.overall_state
    }

    pub fn player(&self) -> Player {
        self.next_player
    }

    pub fn board_to_play(&self) -> Option<usize> {
        self.next_board.map(|b| b as usize)
    }

    pub fn at(&self, board: usize, cell: usize) -> CellState {
        self.boards.at(board, cell)
    }

    pub fn board_state(&self, board: usize) -> BoardState {
        self.game_states.at(board)
    }

    pub fn zobrist(&self) -> u64 {
        self.hash
            ^ self
                .next_board
                .map(|b| ZOBRIST_TABLES.next_board[b as usize])
                .unwrap_or(0)
    }

    pub fn bound_depth(&self) -> usize {
        let mut done = self.game_states.donebits();
        let mut bound: usize = 0;
        // done: u9
        for row in self.boards.rows.iter() {
            let mut free = !(row.x | row.o) & ((1 << 27) - 1);
            if done & 1 != 0 {
                free &= !BOARD_MASK;
            }
            if done & 2 != 0 {
                free &= !(BOARD_MASK << 9);
            }
            if done & 4 != 0 {
                free &= !(BOARD_MASK << 18);
            }
            done >>= 3;
            bound += free.count_ones() as usize;
        }
        bound
    }
}

impl Hash for Game {
    fn hash<H>(&self, h: &mut H)
    where
        H: Hasher,
    {
        h.write_u64(self.zobrist());
    }
}

struct MoveIterator<'a> {
    game: &'a Game,
    board: usize,
    mask: u32,
    cell: usize,
}

impl<'a> MoveIterator<'a> {
    fn from_game(game: &'a Game) -> Self {
        let board = match game.board_to_play() {
            Some(b) => b,
            None => (0..9).find(|b| game.game_states.in_play(*b)).unwrap_or(9),
        };
        MoveIterator {
            game: game,
            cell: 0,
            mask: game.boards.mask(board),
            board: board,
        }
    }
}

impl<'a> Iterator for MoveIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cell >= 9 {
                if let Some(_) = self.game.board_to_play() {
                    return None;
                }
                self.board += 1;
                if !self.game.game_states.in_play(self.board) {
                    continue;
                }
                if self.board >= 9 {
                    return None;
                }
                self.cell = 0;
                self.mask = self.game.boards.mask(self.board);
            }

            let bit = self.mask & 1 == 0;
            let cell = self.cell;
            self.mask >>= 1;
            self.cell += 1;
            if bit {
                return Some(Move::from_coords(self.board, cell));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::collections::hash_map::HashMap;
    use std::collections::VecDeque;
    use std::hash::{Hash, Hasher};

    use super::*;

    fn board(moves: &[(usize, usize)]) -> Game {
        let mut g = Game::new();
        for (i, m) in moves.iter().enumerate() {
            match g.make_move(Move::from_coords(m.0, m.1)) {
                Ok(g_) => g = g_,
                Err(e) => panic!("move failed: {:?}: {:?}\n{}", m, e, &g),
            }
            let at = g.at(m.0, m.1);
            if at != CellState::Played(g.player().other()) {
                panic!(
                    "board(): consistency failure i={} m={:?} bits={:?} at={:?}",
                    i, m, g.boards, at
                );
            }
        }
        g
    }

    #[test]
    fn test_move() {
        let g = Game::new();
        match g.make_move(Move::from_coords(1, 6)) {
            Ok(gg) => {
                assert_eq!(gg.at(1, 6), CellState::Played(Player::X));
                assert_eq!(gg.at(0, 0), CellState::Empty);
                for i in 0..9 {
                    assert_eq!(gg.board_state(i), BoardState::InPlay);
                }
            }
            Err(e) => {
                panic!("move failed: {:?}", e);
            }
        };
    }

    #[test]
    fn test_sub_win() {
        let g = board(&[(0, 4), (4, 0), (0, 2), (2, 0), (0, 6)]);
        assert_eq!(g.board_state(0), BoardState::Won(Player::X));

        let g = board(&[(0, 4), (4, 6), (6, 4), (4, 7), (7, 4), (4, 8)]);
        assert_eq!(g.board_state(4), BoardState::Won(Player::O));
    }

    #[test]
    fn test_sub_draw() {
        let g = board(&[
            (0, 2),
            (2, 0),
            (0, 4),
            (4, 0),
            (0, 7),
            (7, 0),
            (0, 0),
            (0, 1),
            (1, 0),
            (0, 3),
            (3, 0),
            (0, 5),
            (5, 0),
            (0, 6),
            (6, 0),
            (0, 8),
        ]);
        assert_eq!(g.board_state(0), BoardState::Drawn);
    }

    #[test]
    fn test_move_anywhere() {
        let g = board(&[
            (0, 2),
            (2, 0),
            (0, 4),
            (4, 0),
            (0, 7),
            (7, 0),
            (0, 0),
            (0, 1),
            (1, 0),
            (0, 3),
            (3, 0),
            (0, 5),
            (5, 0),
            (0, 6),
            (6, 0),
            (0, 8),
            (8, 0),
        ]);
        for b in &[1, 2, 3, 4, 5, 6, 7, 8] {
            let r = g.make_move(Move::from_coords(*b, *b));
            if let Err(e) = r {
                panic!("Disallowed move: ({}, {}): {:?}", b, b, e);
            }
        }
        assert_eq!(g.board_state(0), BoardState::Drawn);
    }

    #[test]
    fn test_move_anywhere_won() {
        let g = board(&[(0, 4), (4, 0), (0, 2), (2, 0), (0, 6), (6, 0)]);
        for b in &[1, 2, 3, 4, 5, 6, 7, 8] {
            let r = g.make_move(Move::from_coords(*b, *b));
            if let Err(e) = r {
                panic!("Disallowed move: ({}, {}): {:?}", b, b, e);
            }
        }
        let r = g.make_move(Move::from_coords(0, 1));
        if let Ok(_) = r {
            panic!("Can't move into a won board");
        };
        for m in g.all_moves() {
            if let Err(e) = g.make_move(m) {
                panic!("all_moves returned illegal move {}: {:?}", m, e);
            }
        }
    }

    #[test]
    fn test_win() {
        let g = board(&[
            (0, 2),
            (2, 0),
            (0, 4),
            (4, 0),
            (0, 6),
            (6, 1),
            (1, 2),
            (2, 1),
            (1, 4),
            (4, 1),
            (1, 6),
            (6, 2),
            (2, 3),
            (3, 2),
            (2, 4),
            (4, 2),
            (2, 5),
        ]);
        assert_eq!(g.game_state(), BoardState::Won(Player::X));
        let r = g.make_move(Move::from_coords(5, 1));
        match r {
            Ok(_) => panic!("allowed move on won board"),
            Err(e) => {
                assert_eq!(e, MoveError::GameOver);
            }
        }
    }

    #[test]
    fn test_move_errors() {
        let g = board(&[(0, 4), (4, 0)]);
        let cases = &[
            (1, 4, MoveError::WrongBoard),
            (0, 4, MoveError::NotEmpty),
            (0, 10, MoveError::OutOfBounds),
            (9, 3, MoveError::OutOfBounds),
        ];
        for (board, square, expect) in cases {
            let m = g.make_move(Move::from_coords(*board, *square));
            match m {
                Ok(_) => panic!("Move succeeded, expected {:?}", expect),
                Err(e) => {
                    if e != *expect {
                        panic!(
                            "Move returned wrong error: got {:?}, expected {:?}",
                            e, expect
                        )
                    }
                }
            }
        }
    }

    #[test]
    fn test_all_moves() {
        let cases = &[
            (Game::new(), 81),
            (board(&[(0, 4)]), 9),
            (board(&[(4, 4)]), 8),
        ];
        for (board, count) in cases {
            assert_eq!(*count, board.all_moves().collect::<Vec<Move>>().len());
            for m in board.all_moves() {
                if let Err(e) = board.make_move(m) {
                    panic!("Bad move: {:?}: {:?})", m, e);
                }
            }
        }
    }

    #[test]
    fn test_bound_depth() {
        let cases = &[
            (Game::new(), 81),
            (board(&[(0, 0)]), 80),
            (board(&[(0, 2), (2, 0), (0, 4), (4, 0)]), 77),
            (board(&[(0, 2), (2, 0), (0, 4), (4, 0), (0, 6)]), 70),
        ];
        for (i, tc) in cases.iter().enumerate() {
            assert_eq!(tc.0.bound_depth(), tc.1, "idx: {}", i);
        }
    }

    fn hash_value<H>(v: &H) -> u64
    where
        H: Hash,
    {
        let mut hasher = DefaultHasher::new();
        Hash::hash(v, &mut hasher);
        Hasher::finish(&hasher)
    }

    #[test]
    fn test_eq() {
        let cases = &[
            (Game::new(), 0),
            (Game::new(), 0),
            (board(&[(4, 0), (0, 4), (4, 1), (1, 4)]), 1),
            (board(&[(4, 1), (1, 4), (4, 0), (0, 4)]), 1),
            (board(&[(4, 0), (0, 4), (4, 1), (1, 4), (4, 4)]), 2),
            (board(&[(7, 6), (6, 8), (8, 6), (6, 7)]), 3),
            (board(&[(8, 6), (6, 7), (7, 6), (6, 8)]), 4),
        ];
        for l in cases.iter() {
            for r in cases.iter() {
                assert_eq!(l.1 == r.1, l.0 == r.0);
                assert_eq!(l.1 == r.1, hash_value(&l.0) == hash_value(&r.0));
            }
        }
    }

    const TEST_HASH_SAMPLES: usize = 1_000_000;

    #[test]
    fn test_hash() {
        let mut positions = Vec::new();
        let mut queue = VecDeque::new();
        queue.push_back(Game::new());
        while positions.len() < TEST_HASH_SAMPLES {
            let g = queue.pop_front().unwrap();
            positions.push(g.clone());
            if let BoardState::InPlay = g.game_state() {
                for m in g.all_moves() {
                    let gg = g.make_move(m).expect("all_moves returned illegal move");
                    queue.push_back(gg);
                }
            }
        }

        positions.sort_by_cached_key(|g: &Game| {
            let mut out = [0 as u64; 5];
            out[0] = g.game_states.bits as u64;
            out[1] = (g.boards.rows[0].x as u64) << 32 | (g.boards.rows[0].o as u64);
            out[2] = (g.boards.rows[1].x as u64) << 32 | (g.boards.rows[1].o as u64);
            out[3] = (g.boards.rows[2].x as u64) << 32 | (g.boards.rows[2].o as u64);
            out[4] = match g.next_board {
                Some(i) => i as u64,
                None => 0xff,
            };
            out
        });
        let mut i = 0;
        while i < positions.len() - 1 {
            assert_eq!(
                positions[i] == positions[i + 1],
                hash_value(&positions[i]) == hash_value(&positions[i + 1],),
                "i={} l={:?} r={:?}",
                i,
                &positions[i],
                &positions[i + 1],
            );
            i += 1;
        }

        let mut grp: HashMap<u64, Vec<&Game>> = HashMap::new();
        for p in positions.iter() {
            grp.entry(hash_value(p))
                .or_insert_with(|| Vec::new())
                .push(&p);
        }
        for (_h, its) in grp.iter() {
            for p1 in its.iter() {
                for p2 in its.iter() {
                    assert_eq!(p1, p2, "{:?} != {:?} but they hash equal", &p1, &p2);
                }
            }
        }
    }
}
