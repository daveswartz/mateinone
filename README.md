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

    Board(White,Vector(Piece(White,Pawn,Square(File(0),Rank(1)),false), Piece(White,Pawn,Square(File(1),Rank(1)),false), Piece(White,Pawn,Square(File(2),Rank(1)),false), Piece(White,Pawn,Square(File(3),Rank(1)),false), Piece(White,Pawn,Square(File(4),Rank(1)),false), Piece(White,Pawn,Square(File(5),Rank(1)),false), Piece(White,Pawn,Square(File(6),Rank(1)),false), Piece(White,Pawn,Square(File(7),Rank(1)),false), Piece(White,Rook,Square(File(0),Rank(0)),false), Piece(White,Rook,Square(File(7),Rank(0)),false), Piece(White,Knight,Square(File(1),Rank(0)),false), Piece(White,Knight,Square(File(6),Rank(0)),false), Piece(White,Bishop,Square(File(2),Rank(0)),false), Piece(White,Bishop,Square(File(5),Rank(0)),false), Piece(White,King,Square(File(4),Rank(0)),false), Piece(White,Queen,Square(File(3),Rank(0)),false), Piece(Black,Pawn,Square(File(0),Rank(6)),false), Piece(Black,Pawn,Square(File(1),Rank(6)),false), Piece(Black,Pawn,Square(File(2),Rank(6)),false), Piece(Black,Pawn,Square(File(3),Rank(6)),false), Piece(Black,Pawn,Square(File(4),Rank(6)),false), Piece(Black,Pawn,Square(File(5),Rank(6)),false), Piece(Black,Pawn,Square(File(6),Rank(6)),false), Piece(Black,Pawn,Square(File(7),Rank(6)),false), Piece(Black,Rook,Square(File(0),Rank(7)),false), Piece(Black,Rook,Square(File(7),Rank(7)),false), Piece(Black,Knight,Square(File(1),Rank(7)),false), Piece(Black,Knight,Square(File(6),Rank(7)),false), Piece(Black,Bishop,Square(File(2),Rank(7)),false), Piece(Black,Bishop,Square(File(5),Rank(7)),false), Piece(Black,King,Square(File(4),Rank(7)),false), Piece(Black,Queen,Square(File(3),Rank(7)),false)))

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

Note: in the terminal the varying width of unicode characters is handled properly.

The `print` implicit method of the `GithubFlavoredMarkdownPrinter` is used to generate the markdown representing the board for subsequent examples.

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

 a | b | c | d | e | f | g | h | ∙
:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:
 ♜ |   | ♝ | ♛ | ♚ | ♝ |   | ♜ | **8**
 ♟ | ♟ | ♟ | ♟ |   | ♕ | ♟ | ♟ | **7**
   |   | ♞ |   |   | ♞ |   |   | **6**
   |   |   |   | ♟ |   |   |   | **5**
   |   | ♗ |   | ♙ |   |   |   | **4**
   |   |   |   |   |   |   |   | **3**
 ♙ | ♙ | ♙ | ♙ |   | ♙ | ♙ | ♙ | **2**
 ♖ | ♘ | ♗ |   | ♔ |   | ♘ | ♖ | **1**

Both sides play to quickly **castle kingside**:

1. Nf3 Nf6
2. g3 g6
3. Bh3 Bh6
4. O-O O-O

```scala
println(Board.initial.move(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`).get.print)
```

 a | b | c | d | e | f | g | h | ∙
:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:
 ♜ | ♞ | ♝ | ♛ |   | ♜ | ♚ |   | **8**
 ♟ | ♟ | ♟ | ♟ | ♟ | ♟ |   | ♟ | **7**
   |   |   |   |   | ♞ | ♟ | ♝ | **6**
   |   |   |   |   |   |   |   | **5**
   |   |   |   |   |   |   |   | **4**
   |   |   |   |   | ♘ | ♙ | ♗ | **3**
 ♙ | ♙ | ♙ | ♙ | ♙ | ♙ |   | ♙ | **2**
 ♖ | ♘ | ♗ | ♕ |   | ♖ | ♔ |   | **1**

Make some **exchanges**:

1. d4 e5
2. dxe5 d6
3. Bg5 dxe5
4. Bxd8

```scala
println(Board.initial.move(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8).get.print)
```

 a | b | c | d | e | f | g | h | ∙
:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:
 ♜ | ♞ | ♝ | ♗ | ♚ | ♝ | ♞ | ♜ | **8**
 ♟ | ♟ | ♟ |   |   | ♟ | ♟ | ♟ | **7**
   |   |   |   |   |   |   |   | **6**
   |   |   |   | ♟ |   |   |   | **5**
   |   |   |   |   |   |   |   | **4**
   |   |   |   |   |   |   |   | **3**
 ♙ | ♙ | ♙ |   | ♙ | ♙ | ♙ | ♙ | **2**
 ♖ | ♘ |   | ♕ | ♔ | ♗ | ♘ | ♖ | **1**

### Generate Moves

Generate the **opening moves** and print them using the  the `toString` method of `Move`:

```scala
println(Board.initial.moves)
```

Outputs the following:

    Set(c2->c3, f2->f3, f2->f4, b2->b4, a2->a3, c2->c4, d2->d3, b2->b3, g2->g4, h2->h3, b1->c3, e2->e4, d2->d4, g2->g3, a2->a4, h2->h4, g1->h3, b1->a3, g1->f3, e2->e3)

Scripts ♖
---------

The script directory contains all the code above in **[basic.scala](script/basic.scala)**

The **[computer-computer-game.scala](script/computer-computer-game.scala)** script plays both sides until the game is won or drawn.
