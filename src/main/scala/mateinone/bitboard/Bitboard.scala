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
    val color = sideToMove
    val captured = if (m.capture) pieceAt(to) else PieceNone
    
    positionHistory = hash :: positionHistory
    history = State(captured, castleRights, enPassantSq, halfMoveClock, hash, evalScore) :: history

    if (m.capture) {
      if (m.enPassant) {
        val capturedPawnSq = if (color == White) to - 8 else to + 8
        removePiece(capturedPawnSq)
      } else {
        removePiece(to)
      }
    }
    
    removePiece(from)
    
    if (m.promo != PieceNone) {
      putPiece(color, m.promo, to)
    } else {
      putPiece(color, piece, to)
    }

    // Handle Castling (Move Rook)
    if (m.castle) {
      if (to == G1) { removePiece(H1); putPiece(White, Rook, F1) }
      else if (to == C1) { removePiece(A1); putPiece(White, Rook, D1) }
      else if (to == G8) { removePiece(H8); putPiece(Black, Rook, F8) }
      else if (to == C8) { removePiece(A8); putPiece(Black, Rook, D8) }
    }

    // Hash updates for state changes
    if (enPassantSq != SquareNone) hash ^= Zobrist.enPassant(fileOf(enPassantSq))
    hash ^= Zobrist.castling(castleRights & 3) // WK/WQ
    hash ^= Zobrist.castling((castleRights >> 2) & 3) // BK/BQ
    
    // Update state
    sideToMove ^= 1
    hash ^= Zobrist.sideToMove
    
    // Update halfMoveClock
    if (piece == Pawn || m.capture) halfMoveClock = 0
    else halfMoveClock += 1
    
    if (color == Black) fullMoveNumber += 1
    
    // Update Castle Rights
    castleRights &= castleRightsUpdateMask(from)
    castleRights &= castleRightsUpdateMask(to)
    
    // Reset EP square unless double push
    enPassantSq = SquareNone
    if (piece == Pawn && Math.abs(to - from) == 16) {
      enPassantSq = if (color == White) from + 8 else from - 8
      hash ^= Zobrist.enPassant(fileOf(enPassantSq))
    }
    
    hash ^= Zobrist.castling(castleRights & 3)
    hash ^= Zobrist.castling((castleRights >> 2) & 3)
    
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
      if (m.enPassant) {
        val capturedPawnSq = if (color == White) to - 8 else to + 8
        putPiece(sideToMove, state.capturedPiece, capturedPawnSq)
      } else {
        putPiece(sideToMove, state.capturedPiece, to)
      }
    }

    // Handle Castling (Move Rook Back)
    if (m.castle) {
      if (to == G1) { removePiece(F1); putPiece(White, Rook, H1) }
      else if (to == C1) { removePiece(D1); putPiece(White, Rook, A1) }
      else if (to == G8) { removePiece(F8); putPiece(Black, Rook, H8) }
      else if (to == C8) { removePiece(D8); putPiece(Black, Rook, A8) }
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

  def isFiftyMoveRule: Boolean = halfMoveClock >= 100

  def isInsufficientMaterial: Boolean = {
    // No pawns, rooks, or queens
    if ((pieceBB(White)(Pawn) | pieceBB(Black)(Pawn) | 
         pieceBB(White)(Rook) | pieceBB(Black)(Rook) | 
         pieceBB(White)(Queen) | pieceBB(Black)(Queen)) != 0) return false
    
    val wKnights = java.lang.Long.bitCount(pieceBB(White)(Knight))
    val bKnights = java.lang.Long.bitCount(pieceBB(Black)(Knight))
    val wBishops = java.lang.Long.bitCount(pieceBB(White)(Bishop))
    val bBishops = java.lang.Long.bitCount(pieceBB(Black)(Bishop))
    
    // K vs K
    if (wKnights == 0 && bKnights == 0 && wBishops == 0 && bBishops == 0) return true
    
    // K+N vs K or K vs K+N
    if (wKnights + wBishops <= 1 && bKnights + bBishops == 0) return true
    if (bKnights + bBishops <= 1 && wKnights + wBishops == 0) return true
    
    // K+B vs K+B (if bishops are on same color squares - simplified for now)
    if (wKnights == 0 && bKnights == 0 && wBishops == 1 && bBishops == 1) {
       // Check square colors... for now just return true to match OO engine spirit
       return true 
    }
    
    false
  }
}

object Bitboard {
  def fromFen(fen: String): Bitboard = {
    val b = new Bitboard()
    val parts = fen.split(" ")
    val ranks = parts(0).split("/")
    
    for (r <- 0 until 8) {
      var f = 0
      for (char <- ranks(7 - r)) {
        if (char.isDigit) {
          f += char.asDigit
        } else {
          val color = if (char.isUpper) White else Black
          val pieceType = char.toLower match {
            case 'p' => Pawn
            case 'n' => Knight
            case 'b' => Bishop
            case 'r' => Rook
            case 'q' => Queen
            case 'k' => King
          }
          b.putPiece(color, pieceType, squareIndex(f, r))
          f += 1
        }
      }
    }
    
    b.sideToMove = if (parts(1) == "w") White else Black
    
    b.castleRights = 0
    if (parts(2).contains('K')) b.castleRights |= CastleWK
    if (parts(2).contains('Q')) b.castleRights |= CastleWQ
    if (parts(2).contains('k')) b.castleRights |= CastleBK
    if (parts(2).contains('q')) b.castleRights |= CastleBQ
    
    if (parts(3) != "-") {
      val f = parts(3)(0) - 'a'
      val r = parts(3)(1).asDigit - 1
      b.enPassantSq = squareIndex(f, r)
    }
    
    b.halfMoveClock = parts(4).toInt
    b.fullMoveNumber = parts(5).toInt
    
    b.updateOccupancy()
    
    // Initialize hash and evalScore
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
    if (b.enPassantSq != SquareNone) b.hash ^= Zobrist.enPassant(fileOf(b.enPassantSq))
    b.hash ^= Zobrist.castling(b.castleRights & 3)
    b.hash ^= Zobrist.castling((b.castleRights >> 2) & 3)
    
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

  def initial: Bitboard = fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
}
