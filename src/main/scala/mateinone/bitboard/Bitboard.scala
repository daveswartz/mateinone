package mateinone.bitboard

import Constants._
import mateinone.Zobrist
import java.lang.Long.numberOfTrailingZeros

class Bitboard {
  val pieceBB: Array[Array[Long]] = Array.ofDim[Long](2, 6)
  val occupancy: Array[Long] = new Array[Long](3)
  val pieceAt: Array[Int] = Array.fill(64)(PieceNone)
  val colorAt: Array[Int] = Array.fill(64)(ColorNone)

  var sideToMove: Int = White
  var castleRights: Int = CastleAll
  var enPassantSq: Int = SquareNone
  var halfMoveClock: Int = 0
  var fullMoveNumber: Int = 1
  var hash: Long = 0L
  var evalScore: Int = 0
  var positionHistory: List[Long] = Nil

  private case class State(
    capturedPiece: Int,
    oldCastleRights: Int,
    oldEnPassantSq: Int,
    oldHalfMoveClock: Int,
    oldHash: Long,
    oldEvalScore: Int
  )
  private var history: List[State] = Nil

  def updateOccupancy(): Unit = {
    occupancy(White) = pieceBB(White).reduce(_ | _)
    occupancy(Black) = pieceBB(Black).reduce(_ | _)
    occupancy(2) = occupancy(White) | occupancy(Black)
  }

  def putPiece(color: Int, pieceType: Int, sq: Int): Unit = {
    pieceBB(color)(pieceType) |= (1L << sq)
    pieceAt(sq) = pieceType
    colorAt(sq) = color
    hash ^= Zobrist.pieces(color)(pieceType)(sq)
    
    // Incremental Eval (Note: this is raw score, side-independent)
    val sideMult = if (color == White) 1 else -1
    evalScore += sideMult * BitboardEvaluator.pieceValue(pieceType, color, sq, false) // Assume middle-game for incremental
  }

  def removePiece(sq: Int): Unit = {
    val color = colorAt(sq)
    val pieceType = pieceAt(sq)
    if (color != ColorNone) {
      pieceBB(color)(pieceType) &= ~(1L << sq)
      pieceAt(sq) = PieceNone
      colorAt(sq) = ColorNone
      hash ^= Zobrist.pieces(color)(pieceType)(sq)
      
      val sideMult = if (color == White) 1 else -1
      evalScore -= sideMult * BitboardEvaluator.pieceValue(pieceType, color, sq, false)
    }
  }

  def makeMove(m: Move): Unit = {
    val from = m.from
    val to = m.to
    val piece = m.piece
    val captured = if (m.capture) pieceAt(to) else PieceNone
    
    positionHistory = hash :: positionHistory
    history = State(captured, castleRights, enPassantSq, halfMoveClock, hash, evalScore) :: history

    if (m.capture) removePiece(to)
    
    val color = sideToMove
    removePiece(from)
    
    if (m.promo != PieceNone) {
      putPiece(color, m.promo, to)
    } else {
      putPiece(color, piece, to)
    }

    // Hash updates for state changes
    if (enPassantSq != SquareNone) hash ^= Zobrist.enPassant(fileOf(enPassantSq))
    // TODO: Update hash for castleRights change
    
    // Update state
    sideToMove ^= 1
    hash ^= Zobrist.sideToMove
    
    if (color == Black) fullMoveNumber += 1
    
    // Reset EP square unless double push
    enPassantSq = SquareNone
    if (piece == Pawn && Math.abs(to - from) == 16) {
      enPassantSq = if (color == White) from + 8 else from - 8
      hash ^= Zobrist.enPassant(fileOf(enPassantSq))
    }
    
    updateOccupancy()
  }

  def unmakeMove(m: Move): Unit = {
    val state = history.head
    history = history.tail
    positionHistory = positionHistory.tail

    val from = m.from
    val to = m.to
    val piece = m.piece
    val color = sideToMove ^ 1
    
    removePiece(to)
    putPiece(color, piece, from)

    if (state.capturedPiece != PieceNone) {
      putPiece(sideToMove, state.capturedPiece, to)
    }

    sideToMove = color
    castleRights = state.oldCastleRights
    enPassantSq = state.oldEnPassantSq
    halfMoveClock = state.oldHalfMoveClock
    hash = state.oldHash
    evalScore = state.oldEvalScore
    if (color == Black) fullMoveNumber -= 1
    updateOccupancy()
  }

  def isThreefoldRepetition: Boolean = {
    positionHistory.count(_ == hash) >= 2
  }
}

object Bitboard {
  def initial: Bitboard = {
    val b = new Bitboard()
    // Pawns
    for (i <- 0 until 8) {
      b.putPiece(White, Pawn, squareIndex(i, 1))
      b.putPiece(Black, Pawn, squareIndex(i, 6))
    }
    // Rooks
    b.putPiece(White, Rook, A1); b.putPiece(White, Rook, H1)
    b.putPiece(Black, Rook, A8); b.putPiece(Black, Rook, H8)
    // Knights
    b.putPiece(White, Knight, B1); b.putPiece(White, Knight, G1)
    b.putPiece(Black, Knight, B8); b.putPiece(Black, Knight, G8)
    // Bishops
    b.putPiece(White, Bishop, C1); b.putPiece(White, Bishop, F1)
    b.putPiece(Black, Bishop, C8); b.putPiece(Black, Bishop, F8)
    // Queens
    b.putPiece(White, Queen, D1); b.putPiece(Black, Queen, D8)
    // Kings
    b.putPiece(White, King, E1); b.putPiece(Black, King, E8)
    
    b.updateOccupancy()
    
    // Initialize hash correctly
    b.hash = 0L
    for (c <- 0 to 1; pt <- 0 to 5) {
      var bb = b.pieceBB(c)(pt)
      while (bb != 0) {
        val sq = numberOfTrailingZeros(bb)
        b.hash ^= Zobrist.pieces(c)(pt)(sq)
        bb &= (bb - 1)
      }
    }
    if (b.sideToMove == Black) b.hash ^= Zobrist.sideToMove
    // TODO: Initial castle rights hash
    
    // Initialize evalScore correctly
    b.evalScore = 0
    for (c <- 0 to 1; pt <- 0 to 5) {
      val sideMult = if (c == White) 1 else -1
      var bb = b.pieceBB(c)(pt)
      while (bb != 0) {
        val sq = numberOfTrailingZeros(bb)
        b.evalScore += sideMult * BitboardEvaluator.pieceValue(pt, c, sq, false)
        bb &= (bb - 1)
      }
    }
    
    b
  }
}
