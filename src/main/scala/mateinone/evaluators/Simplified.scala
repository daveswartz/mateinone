package mateinone.evaluators

import mateinone._

/**
 * Score is based on material unless mated.
 *
 * Source: https://chessprogramming.wikispaces.com/Simplified+evaluation+function
 */
object Simplified extends Evaluator {

  private val pawnSquareTable = Vector(
      0,   0,   0,   0,   0,   0,   0,   0,
     50,  50,  50,  50,  50,  50,  50,  50,
     10,  10,  20,  30,  30,  20,  10,  10,
      5,   5,  10,  25,  25,  10,   5,   5,
      0,   0,   0,  20,  20,   0,   0,   0,
      5,  -5, -10,   0,   0, -10,  -5,   5,
      5,  10,  10, -20, -20,  10,  10,   5,
      0,   0,   0,   0,   0,   0,   0,   0
  )

  private val knightSquareTable = Vector(
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20,   0,   0,   0,   0, -20, -40,
    -30,   0,  10,  15,  15,  10,   0, -30,
    -30,   5,  15,  20,  20,  15,   5, -30,
    -30,   0,  15,  20,  20,  15,   0, -30,
    -30,   5,  10,  15,  15,  10,   5, -30,
    -40, -20,   0,   5,   5,   0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50
  )

  private val bishopSquareTable = Vector(
    -20, -10, -10, -10, -10, -10, -10, -20,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -10,   0,   5,  10,  10,   5,   0, -10,
    -10,   5,   5,  10,  10,   5,   5, -10,
    -10,   0,  10,  10,  10,  10,   0, -10,
    -10,  10,  10,  10,  10,  10,  10, -10,
    -10,   5,   0,   0,   0,   0,   5, -10,
    -20, -10, -10, -10, -10, -10, -10, -20
  )

  private val rookSquareTable = Vector(
      0,   0,   0,   0,   0,   0,   0,   0,
      5,  10,  10,  10,  10,  10,  10,   5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
     -5,   0,   0,   0,   0,   0,   0,  -5,
      0,   0,   0,   5,   5,   0,   0,   0
  )

  private val queenSquareTable = Vector(
    -20, -10, -10,  -5,  -5, -10, -10, -20,
    -10,   0,   0,   0,   0,   0,   0, -10,
    -10,   0,   5,   5,   5,   5,   0, -10,
     -5,   0,   5,   5,   5,   5,   0,  -5,
      0,   0,   5,   5,   5,   5,   0,  -5,
    -10,   5,   5,   5,   5,   5,   0, -10,
    -10,   0,   5,   0,   0,   0,   0, -10,
    -20, -10, -10,  -5,  -5, -10, -10, -20
  )

  private val kingMiddleSquareTable = Vector(
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -20, -30, -30, -40, -40, -30, -30, -20,
    -10, -20, -20, -20, -20, -20, -20, -10,
     20,  20,   0,   0,   0,   0,  20,  20,
     20,  30,  10,   0,   0,  10,  30,  20
  )

  private val kingEndSquareTable = Vector(
    -50, -40, -30, -20, -20, -30, -40, -50,
    -30, -20, -10,   0,   0, -10, -20, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  30,  40,  40,  30, -10, -30,
    -30, -10,  20,  30,  30,  20, -10, -30,
    -30, -30,   0,   0,   0,   0, -30, -30,
    -50, -30, -30, -30, -30, -30, -30, -50
  )

  private def sides(pieceSquareTable: Vector[Int], pieceValue: Int) = {
    val whitesView = Square.Squares.reverse.flatten
    val blacksView = Square.Squares.flatten
    (blacksView.zip(pieceSquareTable.map(_ + pieceValue)).toMap, whitesView.zip(pieceSquareTable.map(-_ - pieceValue)).toMap)
  }

  private val (whitePawn, blackPawn) = sides(pawnSquareTable, 100)
  private val (whiteKnight, blackKnight) = sides(knightSquareTable, 320)
  private val (whiteBishop, blackBishop) = sides(bishopSquareTable, 330)
  private val (whiteRook, blackRook) = sides(rookSquareTable, 500)
  private val (whiteQueen, blackQueen) = sides(queenSquareTable, 900)
  private val (whiteKingMiddle, blackKingMiddle) = sides(kingMiddleSquareTable, 0)
  private val (whiteKingEnd, blackKingEnd) = sides(kingEndSquareTable, 0)

  def pieceValue(t: PieceType, color: Color, square: Square, endGame: Boolean): Int = t match {
    case Pawn => if (color == White) whitePawn(square) else blackPawn(square)
    case Knight => if (color == White) whiteKnight(square) else blackKnight(square)
    case Bishop => if (color == White) whiteBishop(square) else blackBishop(square)
    case Rook => if (color == White) whiteRook(square) else blackRook(square)
    case Queen => if (color == White) whiteQueen(square) else blackQueen(square)
    case King => if (color == White) { if (endGame) whiteKingEnd(square) else whiteKingMiddle(square) }
                 else { if (endGame) blackKingEnd(square) else blackKingMiddle(square) }
  }

  def calculateFullScore(b: Board): Int = {
    val endGame = isEndgame(b)
    valueForSide(b.same, endGame) + valueForSide(b.opponent, endGame)
  }

  private def valueForSide(side: Side, endGame: Boolean): Int = {
    var result = 0
    side.pawns.foreach(result += pieceValue(Pawn, side.color, _, endGame))
    side.knights.foreach(result += pieceValue(Knight, side.color, _, endGame))
    side.bishops.foreach(result += pieceValue(Bishop, side.color, _, endGame))
    side.rooks.foreach(result += pieceValue(Rook, side.color, _, endGame))
    side.queens.foreach(result += pieceValue(Queen, side.color, _, endGame))
    side.kings.foreach(result += pieceValue(King, side.color, _, endGame))
    result
  }

  private def isEndgame(b: Board) = {
    def inEndgame(side: Side) = side.queens.isEmpty || side.rooks.isEmpty && side.knights.size + side.bishops.size <= 1
    inEndgame(b.same) && inEndgame(b.opponent)
  }

  def evaluate(b: Board, depth: Int): Int = if (b.isCheckmate) {
    if (b.same.color == White) -20000 + depth else 20000 - depth
  } else {
    // For now, return the pre-calculated score from the board. 
    // We'll update Board to store this.
    b.evalScore
  }

}
