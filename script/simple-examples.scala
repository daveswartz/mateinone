import mateinone._
import Square._
import SimpleMove._
import Castle._

// Create a chess board in the initial state.
val initial_board = Board()

// Who's turn is it? White's
val initial_turn = initial_board.turn

// Select some piece.
val some_piece = initial_board.pieces.head

// Find the square for each white pawn.
val white_pawn_squares = initial_board.pieces.filter(_.side == White).filter(_.pieceType == Pawn).map(_.square)

// Find the piece at a2 in the board.
val piece_at_a2 = initial_board.pieceAt(a2).get

// Move the white pawn to a3.
val board_after_a3 = initial_board.move(a2->a3).get

// Move the black pawn to a6.
val board_after_a4 = board_after_a3.move(a7->a6).get

// Find the piece that changed as a result of the move. In this case, `black_pawn_a7` is a `Set` containing only the
// pawn at a7.
val black_pawn_a7 = board_after_a4.pieces.diff(board_after_a3.pieces).head

// The `move` method takes one repeated parameter; therefore, one can specify any number of moves.

// 1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. O-O O-O
val `board_after_O-O` = initial_board.move(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`).get

// Generate all the opening moves.
val initial_moves = initial_board.moves

// Make one of the generated opening moves.
val after_some_move = initial_board.move(initial_board.moves.head).get