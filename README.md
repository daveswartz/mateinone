Mate in one ♔
=============

Chess library entirely written in Scala.

Examples ♕
----------

Create a board in the initial position.

```scala
println(Board())
```

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

Play a simple trap known as the Scholar's mate.

1. e4 e5
2. Qh5 Nc6
3. Bc4 Nf6
4. Qxf7# 1–0

```scala
println(Board().move(e2->e4, e7->e5, d1->h5, b8->c6, c1->c4, g8->f6, h5->f7).get)
```

Both sides play to quickly castle kingside.

1. Nf3 Nf6
2. g3 g6
3. Bh3 Bh6
4. O-O O-O

```scala
println(Board().move(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`).get)
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

Make some exchanges.

1. d4 e5
2. dxe5 d6
3. Bg5 dxe5
4. Bxd8

```scala
println(Board().move(d2->d4, e7->e5, d4->e5, d7->d6, c1->g5, d6->e5, g5->d8).get)
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

Generate the opening moves.

```scala
println(Board().moves)
```

    Set(c2->c3, f2->f3, f2->f4, b2->b4, a2->a3, c2->c4, d2->d3, b2->b3, g2->g4, h2->h3, b1->c3, e2->e4, d2->d4, g2->g3, a2->a4, h2->h4, g1->h3, b1->a3, g1->f3, e2->e3)

The [script](script) directory contains more examples of how to use the library.

* [random-game.scala](script/random-game.scala) plays both sides in a random game that continues until one side wins;

Developing Locally ♖
--------------------

* Setup the latest version of SBT;
* Clone this repository;
* Setup IntelliJ IDEA 12;
* Run `sbt update gen-idea` from the project root;
* Open the project in IntelliJ IDEA 12.
