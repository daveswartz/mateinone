Mate in one ♔
=============

Chess library entirely written in Scala.

Usage ♕
-------

The library is really easy to use. Just bring the `mateinone` package into scope.

```scala
import mateinone._
```

Alternatively, use `sbt console` and skip the imports as it is configured to pre-import everything.

### The Board

Create a board in the **initial position**:

```scala
println(Board.initial)
```

The `toString` method outputs the following:

    Board(Side(White,Map(h2 -> Pawn, f1 -> Bishop, a1 -> Rook, f2 -> Pawn, e1 -> King, d2 -> Pawn, g2 -> Pawn, e2 -> Pawn, c1 -> Bishop, d1 -> Queen, b2 -> Pawn, a2 -> Pawn, c2 -> Pawn, g1 -> Knight, b1 -> Knight, h1 -> Rook),Map(Knight -> Set(b1, g1), King -> Set(e1), Bishop -> Set(c1, f1), Queen -> Set(d1), Rook -> Set(a1, h1), Pawn -> Set(h2, f2, d2, g2, e2, b2, a2, c2)),Set()),Side(Black,Map(h7 -> Pawn, g8 -> Knight, h8 -> Rook, e8 -> King, g7 -> Pawn, a8 -> Rook, b7 -> Pawn, d7 -> Pawn, e7 -> Pawn, c7 -> Pawn, f8 -> Bishop, c8 -> Bishop, d8 -> Queen, b8 -> Knight, f7 -> Pawn, a7 -> Pawn),Map(Knight -> Set(b8, g8), King -> Set(e8), Bishop -> Set(c8, f8), Queen -> Set(d8), Rook -> Set(a8, h8), Pawn -> Set(h7, g7, b7, d7, e7, c7, f7, a7)),Set()),None,Vector(),0)

### Pretty Print

Print a nicer presentation of the board using the `print` implicit method of `TerminalPrinter`.

```scala
import TerminalPrinter._

println(Board.initial.print)
```

Outputs the following:

<pre>
┌─────────────────┐
│ ♜ ♞ ♝ ♛ ♚ ♝ ♞ ♜ │
│ ♟ ♟ ♟ ♟ ♟ ♟ ♟ ♟ │
│                 │
│                 │
│                 │
│                 │
│ ♙ ♙ ♙ ♙ ♙ ♙ ♙ ♙ │
│ ♖ ♘ ♗ ♕ ♔ ♗ ♘ ♖ │
└─────────────────┘
</pre>

### Make Moves

More things must be brought into scope so that squares and moves can be expressed.

```scala
import Square._
import MoveImplicits._
```

Play a simple trap known as the **Scholar's mate**:

1. e4 e5
2. Qh5 Nc6
3. Bc4 Nf6
4. Qxf7# 1–0

```scala
println(Board.initial.move(E2->E4, E7->E5, D1->H5, B8->C6, F1->C4, G8->F6, H5->F7).get.print)
```

<pre>
┌─────────────────┐
│ ♜   ♝ ♛ ♚ ♝   ♜ │
│ ♟ ♟ ♟ ♟   ♕ ♟ ♟ │
│     ♞     ♞     │
│         ♟       │
│     ♗   ♙       │
│                 │
│ ♙ ♙ ♙ ♙   ♙ ♙ ♙ │
│ ♖ ♘ ♗   ♔   ♘ ♖ │
└─────────────────┘
</pre>

Both sides play to quickly **castle kingside**:

1. Nf3 Nf6
2. g3 g6
3. Bh3 Bh6
4. O-O O-O

```scala
println(Board.initial.move(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`).get.print)
```

<pre>
┌─────────────────┐
│ ♜ ♞ ♝ ♛   ♜ ♚   │
│ ♟ ♟ ♟ ♟ ♟ ♟   ♟ │
│           ♞ ♟ ♝ │
│                 │
│                 │
│           ♘ ♙ ♗ │
│ ♙ ♙ ♙ ♙ ♙ ♙   ♙ │
│ ♖ ♘ ♗ ♕   ♖ ♔   │
└─────────────────┘
</pre>

Make some **exchanges**:

1. d4 e5
2. dxe5 d6
3. Bg5 dxe5
4. Bxd8

```scala
println(Board.initial.move(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8).get.print)
```

<pre>
┌─────────────────┐
│ ♜ ♞ ♝ ♗ ♚ ♝ ♞ ♜ │
│ ♟ ♟ ♟     ♟ ♟ ♟ │
│                 │
│         ♟       │
│                 │
│                 │
│ ♙ ♙ ♙   ♙ ♙ ♙ ♙ │
│ ♖ ♘   ♕ ♔ ♗ ♘ ♖ │
└─────────────────┘
</pre>

### Generate Moves

Generate the **opening moves** and print them using the  the `toString` method of `Move`:

```scala
println(Board.initial.moves)
```

Outputs the following:

    Vector(b1->c3, b1->a3, g1->h3, g1->f3, h2->h4, f2->f4, d2->d4, g2->g4, e2->e4, b2->b4, a2->a4, c2->c4, h2->h3, f2->f3, d2->d3, g2->g3, e2->e3, b2->b3, a2->a3, c2->c3)

Scripts ♖
---------

The script directory contains all the code above in **[basic.scala](script/basic.scala)**

The **[computer-computer-game.scala](script/computer-computer-game.scala)** script plays both sides until the game is won or drawn.
