Mate in one ♔
=============

Chess library entirely written in Scala.

Usage ♕
-------

The library is really easy to use. Just bring some things into scope

```scala
import mateinone._
```

Create a board in the **initial position** and print the board

```scala
println(Board())
```

The code prints the following

    Board(White,Set(Piece(White,Pawn,Square(File(2),Rank(1)),false), ...

Print a nicer presentation of the board in the Github Flavored Markdown format by bringing a `Board.print` method into scope

```scala
import GithubFlavoredMarkdownPrinter._

println(Board().print)
```

The rendered GFM board is below

 a | b | c | d | e | f | g | h | ∙
:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:
 ♜ | ♞ | ♝ | ♛ | ♚ | ♝ | ♞ | ♜ | **8**
 ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | ♟ | **7**
   |   |   |   |   |   |   |   | **6**
   |   |   |   |   |   |   |   | **5**
   |   |   |   |   |   |   |   | **4**
   |   |   |   |   |   |   |   | **3**
 ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | ♙ | **2**
 ♖ | ♘ | ♗ | ♕ | ♔ | ♗ | ♘ | ♖ | **1**

More things must be brought into scope so that squares and moves can be expressed

```scala
import Square._
import MoveImplicits._
```

Play a simple trap known as the **Scholar's mate**

1. e4 e5
2. Qh5 Nc6
3. Bc4 Nf6
4. Qxf7# 1–0

```scala
println(Board().move(E2->E4, E7->E5, D1->H5, B8->C6, F1->C4, G8->F6, H5->F7).get.print)
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

Both sides play to quickly **castle kingside**

1. Nf3 Nf6
2. g3 g6
3. Bh3 Bh6
4. O-O O-O

```scala
println(Board().move(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`).get.print)
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

Make some **exchanges**.

1. d4 e5
2. dxe5 d6
3. Bg5 dxe5
4. Bxd8

```scala
println(Board().move(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8).get.print)
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

Generate the **opening moves**.

```scala
println(Board().moves.map(_.print))
```

The code prints the following

    Set(G2->G3, E2->E3, H2->H4, A2->A3, C2->C3, B2->B4, G1->H3, C2->C4, B2->B3, E2->E4, G1->F3, G2->G4, F2->F4, D2->D3, A2->A4, B1->A3, D2->D4, H2->H3, F2->F3, B1->C3)

Scripts ♖
---------

The script directory contains all the code above in **[basic.scala](script/basic.scala)**

The directory also contains a longer example, **[random-game.scala](script/random-game.scala)**. The script plays both sides of a random game that continues until one side wins.
