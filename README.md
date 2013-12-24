Scala chess move generation and validation

Getting Started
===============

How to run locally:

* Setup the latest version of SBT;
* Clone this repository;
* Setup IntelliJ IDEA 12;
* Run `sbt update gen-idea` from the project root;
* Open the project in IntelliJ IDEA 12;
* Run the *chess.scala* script in the *script* directory; 

The *chess.scala* script randomly selects a move, prints the move and makes the move in an infinite loop until you terminate it.

Status
==========

The project is in a primordial state. White pieces are placed on a board and moves can be generated and validated according to most of the rules of chess.

Next steps:

* Black Side
* Capture
* Scoring
* Input/Ouput (PGN & FEN)
* Computer Player
* Documentation

Design Principles
=================

The code aims to be approachable rather than perform well. That trade-off may shift with time.
