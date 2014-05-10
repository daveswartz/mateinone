import mateinone._

println(Board.initial)

import TerminalPrinter._

println(Board.initial.print)

import Square._
import MoveImplicits._

println(Board.initial.move(E2->E4, E7->E5, D1->H5, B8->C6, F1->C4, G8->F6, H5->F7).get.print)

println(Board.initial.move(G1->F3, G8->F6, G2->G3, G7->G6, F1->H3, F8->H6, `O-O`, `O-O`).get.print)

println(Board.initial.move(D2->D4, E7->E5, D4->E5, D7->D6, C1->G5, D6->E5, G5->D8).get.print)

println(Board.initial.moves)
