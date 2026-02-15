# Mate In One ♔
A high-performance chess engine and library written entirely in Scala.

## Key Features ♕
- **Advanced Search**: Alpha-Beta pruning with Iterative Deepening.
- **Transposition Table (TT)**: 64-bit Zobrist Hashing with an efficient cache for game state reuse.
- **Move Ordering**:
  - **MVV-LVA**: Most Valuable Victim - Least Valuable Aggressor.
  - **Killer Moves**: Prioritizes moves that caused beta-cutoffs in sibling branches.
- **Pruning Techniques**:
  - **Null Move Pruning**: Early branch termination for dominant positions.
  - **Mate Distance Pruning**: Favors the fastest path to checkmate.
- **Incremental Evaluation**: High-performance evaluation engine with $O(1)$ move overhead using running score updates.
- **Rule Completeness**: Full support for Castling, En Passant, Pawn Promotion, and all Draw rules (Threefold Repetition, Fifty-move Rule, Insufficient Material).

## Performance Milestone 🚀
The engine is optimized for deep search, consistently reaching **Depth 8** in sub-2s per move on standard hardware.

## Usage ♖
Bring the `mateinone` package into scope to access the board and search library.

```scala
import mateinone._
import mateinone.evaluators.Simplified
```

### Finding the Best Move
Use the `Search` module to evaluate positions:

```scala
val board = Board.initial
val depth = 8
val result = Search.nextMove(board, depth, Simplified)

println(s"Best Move: ${result.moves.head}")
println(s"Evaluation: ${result.score / 100.0} pawns")
```

### Pretty Printing
```scala
import TerminalPrinter._
println(Board.initial.print)
```

## Development 🛠

### Running Tests
The project features a comprehensive suite of unit tests for move generation, rules, and search accuracy.
```bash
sbt test
```

### Code Coverage
Generate a detailed coverage report using Scoverage:
```bash
sbt clean coverage test coverageReport
```
Report location: `target/scala-2.13/scoverage-report/index.html`

### Running the Engine Simulation
Play a full game between two instances of the engine:
```bash
echo ":load script/computer-computer-game.scala" | sbt console
```

## License
Distributed under the MIT License. See `LICENSE` for more information.
