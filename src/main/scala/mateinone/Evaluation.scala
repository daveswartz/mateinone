package mateinone

import Square._

object Evaluation {

  private val whiteSquares = Square.Squares.reverse.flatten
  private val blackSquares = Square.Squares.flatten
  private def sides(pieceSquareTable: Vector[Int], pieceValue: Int) =
    (blackSquares.zip(pieceSquareTable.map(_ + pieceValue)).toMap, whiteSquares.zip(pieceSquareTable.map(-_ - pieceValue)).toMap)

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

  private val (whitePawn, blackPawn) = sides(pawnSquareTable, 100)
  private val (whiteKnight, blackKnight) = sides(knightSquareTable, 320)
  private val (whiteBishop, blackBishop) = sides(bishopSquareTable, 330)
  private val (whiteRook, blackRook) = sides(rookSquareTable, 500)
  private val (whiteQueen, blackQueen) = sides(queenSquareTable, 900)
  private val (whiteKingMiddle, blackKingMiddle) = sides(kingMiddleSquareTable, 0)
  private val (whiteKingEnd, blackKingEnd) = sides(kingEndSquareTable, 0)

  private def pieceSquareTable(side: Side, pieceType: PieceType, endGame: Boolean = false): Map[Square, Int] = side match {
    case White =>
      pieceType match {
        case Pawn => whitePawn
        case Knight => whiteKnight
        case Bishop => whiteBishop
        case Rook => whiteRook
        case Queen => whiteQueen
        case King if endGame => whiteKingEnd
        case King => whiteKingMiddle
      }
    case Black =>
      pieceType match {
        case Pawn => blackPawn
        case Knight => blackKnight
        case Bishop => blackBishop
        case Rook => blackRook
        case Queen => blackQueen
        case King if endGame => blackKingEnd
        case King => blackKingMiddle
      }
  }

  private def isEndGame(b: Board) = b.same.queens.isEmpty || b.opponent.queens.isEmpty

  def value(b: Board): Int = {
    val isWhite = b.turn == White
    def v(ofType: Pieces => Set[Square], whiteValues: Map[Square, Int], blackValues: Map[Square, Int]): Int = {
      ofType(b.same).foldLeft(0)(_ + (if (isWhite) whiteValues else blackValues)(_)) + ofType(b.opponent).foldLeft(0)(_ + (if (isWhite) blackValues else whiteValues)(_))
    }
    val endGame = isEndGame(b)
    v(_.pawns, whitePawn, blackPawn) + v(_.knights, whiteKnight, blackKnight) + v(_.bishops, whiteBishop, blackBishop) +
      v(_.rooks, whiteRook, blackRook) + v(_.queens, whiteQueen, blackQueen) +
      v(_.kings, if (endGame) whiteKingEnd else whiteKingMiddle, if (endGame) blackKingEnd else blackKingMiddle)
  }

  private def castleDelta(side: Side, kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square): Int =
    -pieceSquareTable(side, King)(kingStart) - pieceSquareTable(side, King)(rookStart) + pieceSquareTable(side, King)(kingEnd) + pieceSquareTable(side, King)(rookEnd)

  private def captureDelta(b: Board, s: Square): Int = -b.opponent.typeOf(s).fold(0)(pieceSquareTable(b.turn.other, _)(s))

  def deltaValue(b: Board, m: MoveBase): Int = { // TODO if last queen captured then in end game now and reevaluate!
    m match {
      case Move(s: Square, e: Square) =>
        val t = b.same.typeOf(s).get
        -pieceSquareTable(b.turn, t)(s) + pieceSquareTable(b.turn, t)(e) + captureDelta(b, e)
      case Promotion(s: Square, e: Square, t) =>
        -pieceSquareTable(b.turn, Pawn)(s) + pieceSquareTable(b.turn, t)(e) + captureDelta(b, e)
      case `O-O` if b.turn == White => castleDelta(White, E1, G1, H1, F1)
      case `O-O` => castleDelta(Black, E8, G8, H8, F8)
      case `O-O-O` => castleDelta(White, E1, C1, A1, D1)
      case `O-O-O` => castleDelta(Black, E8, C8, A8, D8)
    }
  }

}
