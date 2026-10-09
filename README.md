# MateInOne

MateInOne is a chess engine written in Scala. It speaks the Universal Chess Interface (UCI), so it
plays in chess GUIs, and it also plays in the terminal, against you or against itself.

## Building

MateInOne needs a JDK (tested with 17) and [sbt](https://www.scala-sbt.org/).

```bash
sbt compile
```

## Usage

### In a chess GUI

MateInOne has no graphical board of its own. To play it on one, use a UCI GUI such as
[Cute Chess](https://github.com/cutechess/cutechess) or
[chess-tui](https://github.com/thomas-mauran/chess-tui). A GUI needs a single command that starts
the engine, and `sbt run` won't do, since sbt prints its own output. This writes a script that does:

```bash
sbt compile
CP=$(sbt -batch -error 'export Runtime/fullClasspath')
printf '#!/bin/sh\nexec java -cp "%s" mateinone.Main --uci\n' "$CP" > mateinone-uci
chmod +x mateinone-uci
```

The script runs the classes in `target/`, so compile again after changing the code. Then give the
script to the GUI, for example:

```bash
chess-tui -e ./mateinone-uci
```

It supports `uci`, `isready`, `ucinewgame`, `position` (`startpos` or `fen`, with `moves`), `go`
(`depth`, `movetime`, `wtime`, `btime`, `winc`, `binc`, `movestogo`, `infinite`), `stop` and `quit`.
It has no UCI options. A `go` with no limit searches 8 plies deep. As in Stockfish, a `position` it
can't play (one without a king a side, one where the side to move could capture the king, or an
illegal move) makes it print `info string invalid position: <reason>` and exit with code 1.

### In the terminal

To play against it:

```bash
sbt "run --play"
```

You play White. Choose a piece, then its destination, either by number or by square (such as `e2`,
then `e4`). Enter `q` to quit. The game ends on checkmate, stalemate, threefold repetition, the
fifty-move rule, insufficient material, or a flag falling.

- `--depth N`: the computer searches N plies deep. The default is 12.
- `--time M+S`: adds a clock, M minutes each plus S seconds after each move, such as `5+3`. Without
  `--depth`, the computer searches as deep as its time allows.

To watch it play itself, printing each search and the board after each move:

```bash
sbt "run --depth 8"
```

The default depth is 12.

### As a library

`sbt console` starts a Scala REPL with the engine imported. For example, to find a mate in one:

```scala
import mateinone.bitboard._
import mateinone.bitboard.Constants._
import mateinone.TerminalPrinter._

val board = Bitboard.fromFen("k7/8/1K6/8/8/8/8/7R w - - 0 1")
println(board.print)

val score = BitboardSearch.search(board, 4, -30000, 30000)
val move = BitboardSearch.rootBestMove
println(s"${BitboardSearch.formatScore(score)}: ${squareName(mFrom(move))}${squareName(mTo(move))}")
// Mate in 1: h1h8
```

## Features

### Board

- [Bitboards](https://www.chessprogramming.org/Bitboards), with
  [make/unmake](https://www.chessprogramming.org/Unmake_Move) and
  [Zobrist hashing](https://www.chessprogramming.org/Zobrist_Hashing)
- [Pseudo-legal move generation](https://www.chessprogramming.org/Pseudo-Legal_Move), with a check
  for legality after each move
- Matches the [perft results](https://www.chessprogramming.org/Perft_Results) for the six standard
  positions

### Search

- [Negamax](https://www.chessprogramming.org/Negamax)
  [alpha-beta](https://www.chessprogramming.org/Alpha-Beta) with
  [iterative deepening](https://www.chessprogramming.org/Iterative_Deepening)
- [Aspiration windows](https://www.chessprogramming.org/Aspiration_Windows), in the terminal
- [Quiescence search](https://www.chessprogramming.org/Quiescence_Search): captures, or every move
  when in check
- [Transposition table](https://www.chessprogramming.org/Transposition_Table)
- [Null move pruning](https://www.chessprogramming.org/Null_Move_Pruning)
- [Late move reductions](https://www.chessprogramming.org/Late_Move_Reductions)
- Move ordering by the table's move, [MVV-LVA](https://www.chessprogramming.org/MVV-LVA),
  [killer moves](https://www.chessprogramming.org/Killer_Heuristic) and the
  [history heuristic](https://www.chessprogramming.org/History_Heuristic)
- Draws by [threefold repetition](https://www.chessprogramming.org/Repetitions) and the
  [fifty-move rule](https://www.chessprogramming.org/Fifty-move_Rule)

### Evaluation

- Material and [piece-square tables](https://www.chessprogramming.org/Piece-Square_Tables) from
  Tomasz Michniewski's
  [Simplified Evaluation Function](https://www.chessprogramming.org/Simplified_Evaluation_Function),
  [updated incrementally](https://www.chessprogramming.org/Incremental_Updates)
- A separate king table for the endgame
- [Mop-up evaluation](https://www.chessprogramming.org/Mop-up_Evaluation) in the endgame, which
  drives the losing king to the edge and brings the kings together

## Limitations

- Its strength hasn't been measured, so it has no Elo rating.
- It searches on one thread, with a fixed-size transposition table and no UCI options such as
  `Hash` or `Threads`.
- It has no opening book, endgame tablebases, pondering or Chess960.
- In the terminal, you can only play White.

## Tests

```bash
sbt test
```

The tests cover perft, the rules, the search, UCI and play mode. For a coverage report, in
`target/scala-2.13/scoverage-report/index.html`:

```bash
sbt clean coverage test coverageReport
```

## History

MateInOne began in December 2013 as a Scala library for chess move generation and validation. In
February 2026 it was rewritten around bitboards and became a UCI engine.

## Acknowledgements

- The [Chess Programming Wiki](https://www.chessprogramming.org/), for the techniques above and the
  perft results.
- Tomasz Michniewski, for the Simplified Evaluation Function.
- [Stockfish](https://github.com/official-stockfish/Stockfish), whose handling of UCI and of the
  rules' edge cases this engine follows.

## License

[Mozilla Public License 2.0](LICENSE).
