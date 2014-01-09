import mateinone._
import Square._
import SimpleMove._
import Castle._

val initial_board = Board()

val initial_turn = initial_board.turn

// initial_turn: mateinone.Side = White

val some_piece = initial_board.pieces.head

// some_piece: mateinone.Piece = Piece(Black,Pawn,Square(G,7),false)

val white_pawn_squares = initial_board.pieces.filter(_.side == White).filter(_.pieceType == Pawn).map(_.square)

// white_pawn_squares: scala.collection.immutable.Set[mateinone.Square] = Set(Square(H,2), Square(E,2), Square(F,2), Square(B,2), Square(G,2), Square(A,2), Square(C,2), Square(D,2))

val piece_at_a2 = initial_board.pieceAt(a2).get

// piece_at_a2: mateinone.Piece = Piece(White,Pawn,Square(A,2),false)

val board_after_a3 = initial_board.move(a2->a3).get

val board_after_a4 = board_after_a3.move(a7->a6).get

// Find the piece that changed as a result of the last move.
val black_pawn_a6 = board_after_a4.pieces.diff(board_after_a3.pieces).head

// black_pawn_a6: mateinone.Piece = Piece(Black,Pawn,Square(A,6),true)

// The `move` method takes one repeated parameter; therefore, one can specify any number of moves.

// 1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. O-O O-O
val `board_after_both_side_O-O` = initial_board.move(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`).get

val opening_moves = initial_board.moves

// opening_moves: Set[mateinone.Move] = Set(SimpleMove(Square(G,2),Square(G,4)), SimpleMove(Square(H,2),Square(H,3)), SimpleMove(Square(F,2),Square(F,3)), SimpleMove(Square(B,2),Square(B,4)), SimpleMove(Square(E,2),Square(E,3)), SimpleMove(Square(G,1),Square(F,3)), SimpleMove(Square(B,1),Square(A,3)), SimpleMove(Square(A,2),Square(A,3)), SimpleMove(Square(B,1),Square(C,3)), SimpleMove(Square(H,2),Square(H,4)), SimpleMove(Square(F,2),Square(F,4)), SimpleMove(Square(C,2),Square(C,4)), SimpleMove(Square(E,2),Square(E,4)), SimpleMove(Square(C,2),Square(C,3)), SimpleMove(Square(D,2),Square(D,3)), SimpleMove(Square(B,2),Square(B,3)), SimpleMove(Square(G,1),Square(H,3)), SimpleMove(Square(A,2),Square(A,4)), SimpleMove(Square(G,2),Square(G,3)), SimpleMove(Square(D,2),Square(D,4)))

val opening_moves_for_knights = opening_moves.filter(m => initial_board.pieceAt(m.start).get.pieceType == Knight)

// opening_moves_for_knights: scala.collection.immutable.Set[mateinone.Move] = Set(SimpleMove(Square(G,1),Square(F,3)), SimpleMove(Square(B,1),Square(A,3)), SimpleMove(Square(B,1),Square(C,3)), SimpleMove(Square(G,1),Square(H,3)))

val after_some_move = initial_board.move(opening_moves.head).get
