Mate in one ♔
=============

Chess library entirely written in Scala.

Examples ♕
----------

Create a board.

```scala
Board()
```

Play a simple trap known as the Scholar's mate.

1. e4 e5
2. Qh5 Nc6
3. Bc4 Nf6
4. Qxf7# 1–0

```scala
Board().move(e2->e4, e7->e5, d1->h5, b8->c6, c1->c4, g8->f6, h5->f7)
```

Both sides play to quickly castle kingside.

1. Nf3 Nf6
2. g3 g6
3. Bh3 Bh6
4. O-O O-O

```scala
Board().move(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`)
```

1. d4 e5
2. dxe5 d6
3. Bg5 dxe5
4. Bxd8

```scala
Board().move(d2->d4, e7->e5, d4->e5, d7->d6, c1->g5, d6->e5, g5->d8)
```

Generate the opening moves.

```scala
val opening_moves = Board().moves
```

The [script](script) directory contains more examples of how to use the library.

* [random-game.scala](script/random-game.scala) plays both sides in a random game that continues until one side wins;

Developing Locally ♖
--------------------

* Setup the latest version of SBT;
* Clone this repository;
* Setup IntelliJ IDEA 12;
* Run `sbt update gen-idea` from the project root;
* Open the project in IntelliJ IDEA 12.
