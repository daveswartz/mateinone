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

  private def valueForPawn(color: Color, square: Square): Int = if (color == White) whitePawn(square) else blackPawn(square)
  private def valueForKnight(color: Color, square: Square): Int = if (color == White) whiteKnight(square) else blackKnight(square)
  private def valueForBishop(color: Color, square: Square): Int = if (color == White) whiteBishop(square) else blackBishop(square)
  private def valueForRook(color: Color, square: Square): Int = if (color == White) whiteRook(square) else blackRook(square)
  private def valueForQueen(color: Color, square: Square): Int = if (color == White) whiteQueen(square) else blackQueen(square)
  private def valueForKing(color: Color, endGame: Boolean, square: Square): Int =
    if (color == White) { if (endGame) whiteKingEnd(square) else whiteKingMiddle(square) }
    else { if (endGame) blackKingEnd(square) else blackKingMiddle(square) }

  private def valueForSide(side: Side, endGame: Boolean): Int = {
    var result = 0
    side.pawns.foreach(result += valueForPawn(side.color, _))
    side.knights.foreach(result += valueForKnight(side.color, _))
    side.bishops.foreach(result += valueForBishop(side.color, _))
    side.rooks.foreach(result += valueForRook(side.color, _))
    side.queens.foreach(result += valueForQueen(side.color, _))
    side.kings.foreach(result += valueForKing(side.color, endGame, _))
    result
  }

  private def isEndgame(b: Board) = {
    def inEndgame(side: Side) = side.queens.isEmpty || side.rooks.isEmpty && side.knights.size + side.bishops.size <= 1
    inEndgame(b.same) && inEndgame(b.opponent)
  }

  def evaluate(b: Board): Int = if (b.isCheckmate) {
    if (b.same.color == White) -20000 else 20000
  } else {
    val endGame = isEndgame(b)
    valueForSide(b.same, endGame) + valueForSide(b.opponent, endGame)
  }

}
