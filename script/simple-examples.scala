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
val piece_at_a2 = initial_board.pieceAt(a2)

// Move the white pawn to a3. The move was valid; therefore, `board_after_a3` is of type `Some[Board]`.
val board_after_a3 = initial_board.move(a2->a3)

// Move of the same white pawn to a4. The move was valid; therefore, the `board_after_a4` is of type `Some[Board]`.
val board_after_a4 = board_after_a3.flatMap(_.move(a3->a4))

// Find the piece that changed as a result of the move. In this case, `white_pawn_a4` is a `Set` containing only the
// pawn at a4.
val white_pawn_a4 = board_after_a4.flatMap(a4 => board_after_a3.map(a3 => a4.pieces diff a3.pieces)).get

// The `move` method takes one repeated parameter; therefore, one can specify any number of moves.

// 1. d4 e5 2. dxe5 d6 3. Bg5 dxe5 4. Bxd8
val board_after_Bd8 = initial_board.move(d2->d4, e7->e5, d4->e5, d7->d6, c1->g5, d6->e5, g5->d8)

// 1. Nf3 Nf6 2. g3 g6 3. Bh3 Bh6 4. O-O O-O
val `board_after_O-O` = initial_board.move(g1->f3, g8->f6, g2->g3, g7->g6, f1->h3, f8->h6, `O-O`, `O-O`)

// Generate all the opening moves.
val initial_moves = initial_board.moves

// Make one of the opening moves. The move must be valid; therefore, we can safely call `get` on the `Option`.
val after_some_move = initial_board.move(initial_board.moves.head).get