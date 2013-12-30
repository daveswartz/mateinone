Mate in one ♔
=============

Scala chess move generation and validation.

The project is in a primordial state. White pieces are placed on a board and moves can be generated and validated according to most of the rules of chess.

Getting Started ♕
-----------------

* Setup the latest version of SBT;
* Clone this repository;
* Setup IntelliJ IDEA 12;
* Run `sbt update gen-idea` from the project root;
* Open the project in IntelliJ IDEA 12;
* Run the *chess.scala* script in the *script* directory; 

The *chess.scala* script randomly selects a move, prints the move and makes the move in an infinite loop until you terminate it.

Next steps ♖
------------

* Capture
* Scoring
* Input/Ouput (PGN & FEN)
* Computer Player
* Documentation

Design Principles ♗
-------------------

The code aims to be approachable rather than perform well. That trade-off may shift with time.
