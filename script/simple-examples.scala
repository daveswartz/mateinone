import mateinone._
import Square._
import SimpleMove._
import Castle._

val initial_board = Board()

val initial_turn = initial_board.turn

// initial_turn: mateinone.Side = White

val some_piece = initial_board.pieces.head

// some_piece: mateinone.Piece = Piece(White,♙,c2,false)

val white_pawn_squares = initial_board.pieces.filter(_.side == White).filter(_.pieceType == Pawn).map(_.square)

// white_pawn_squares: scala.collection.immutable.Set[mateinone.Square] = Set(b2, f2, g2, h2, c2, a2, e2, d2)

val board_after_a3 = initial_board.move(a2->a3).get

val board_after_a4 = board_after_a3.move(a7->a6).get

// Find the piece that changed as a result of the last move.
val black_pawn_a6 = board_after_a4.pieces.diff(board_after_a3.pieces).head

// black_pawn_a6: mateinone.Piece = Piece(Black,♙,a6,true)

// The `move` method takes one repeated parameter; therefore, one can specify any number of moves.

// 1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. O-O O-O
val `board_after_both_sides_O-O` = initial_board.move(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`).get

// 1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxd8
val board_after_some_captures = initial_board.move(d2->d4, e7->e5, d4->e5, d7->d6, c1->g5, d6->e5, g5->d8)

val opening_moves = initial_board.moves

// opening_moves: Set[mateinone.Move] = Set(c2->c3, f2->f3, f2->f4, b2->b4, a2->a3, c2->c4, d2->d3, b2->b3, g2->g4, h2->h3, b1->c3, e2->e4, d2->d4, g2->g3, a2->a4, h2->h4, g1->h3, b1->a3, g1->f3, e2->e3)

val after_some_move = initial_board.move(opening_moves.head).get
