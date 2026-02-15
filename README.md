# Mate In One ♔
A high-performance chess engine and library written entirely in Scala.

## Key Features ♕
- **Bitboard Engine**: High-speed, mutable board representation using 64-bit integers for `O(1)` move generation and attacks.
- **Advanced Search**: 
  - **Alpha-Beta Pruning** with Iterative Deepening.
  - **Transposition Table (TT)**: 64-bit Zobrist Hashing cache.
  - **Quiescence Search**: Eliminates horizon effect by searching tactical sequences at leaf nodes.
- **Move Ordering**:
  - **MVV-LVA**: Most Valuable Victim - Least Valuable Aggressor.
  - **Killer Moves**: Prioritizes moves that caused beta-cutoffs.
- **Pruning Techniques**:
  - **Null Move Pruning**: Early branch termination for dominant positions.
  - **Late Move Reductions (LMR)**: Searches less promising moves at shallower depths.
- **Incremental Evaluation**: High-performance evaluation with $O(1)$ updates using running scores.
- **Endgame Heuristics**: "King Herding" logic to force checkmates in simplified endgames.

## Performance Milestone 🚀
The engine is optimized for deep search, consistently reaching **Depth 12** in sub-2s per move on standard hardware (3M+ NPS).

## Usage ♖
Bring the `mateinone.bitboard` package into scope to access the engine.

```scala
import mateinone.bitboard._
import mateinone.TerminalPrinter._
```

### Finding the Best Move
Use the `BitboardSearch` module to evaluate positions:

```scala
val board = Bitboard.initial
val depth = 12
val score = BitboardSearch.search(board, depth, -30000, 30000)

println(s"Evaluation: ${score / 100.0} pawns")
// Best move is stored in Transposition Table
```

### Pretty Printing
```scala
import mateinone.TerminalPrinter._
println(Bitboard.initial.print)
```

## Running the Engine 🎮

### Play vs Computer
Challenge the engine directly from your terminal!
```bash
sbt "run --play --depth 10"
```

### Engine Simulation
Watch the engine play against itself at high depth.
```bash
sbt "run --depth 12"
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

## License
Distributed under the MIT License. See `LICENSE` for more information.
