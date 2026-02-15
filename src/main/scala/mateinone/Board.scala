package mateinone

import Rank._
import Square._
import mateinone.evaluators.Simplified

case class Side(
    color: Color,
    pawns: Set[Square],
    knights: Set[Square],
    bishops: Set[Square],
    rooks: Set[Square],
    queens: Set[Square],
    kings: Set[Square],
    moved: Set[Square] = Set.empty[Square]) {

  def contains(s: Square): Boolean =
    pawns.contains(s)     ||
      knights.contains(s) ||
      bishops.contains(s) ||
      rooks.contains(s)   ||
      queens.contains(s)  ||
      kings.contains(s)

  def add(s: Square, t: PieceType): Side =
    t match {
      case Pawn => copy(pawns = pawns + s, moved = moved + s)
      case Knight => copy(knights = knights + s, moved = moved + s)
      case Bishop => copy(bishops = bishops + s, moved = moved + s)
      case Rook => copy(rooks = rooks + s, moved = moved + s)
      case Queen => copy(queens = queens + s, moved = moved + s)
      case King => copy(kings = kings + s, moved = moved + s)
    }

  def remove(s: Square): Side =
    if(pawns.contains(s)) copy(pawns = pawns - s, moved = moved - s)
    else if(knights.contains(s)) copy(knights = knights - s, moved = moved - s)
    else if(bishops.contains(s)) copy(bishops = bishops - s, moved = moved - s)
    else if(rooks.contains(s)) copy(rooks = rooks - s, moved = moved - s)
    else if(queens.contains(s)) copy(queens = queens - s, moved = moved - s)
    else copy(kings = kings - s, moved = moved - s)

  def typeAt(s: Square): Option[PieceType] =
    if (pawns.contains(s)) Some(Pawn)
    else if (knights.contains(s)) Some(Knight)
    else if (bishops.contains(s)) Some(Bishop)
    else if (rooks.contains(s)) Some(Rook)
    else if (queens.contains(s)) Some(Queen)
    else if (kings.contains(s)) Some(King)
    else None

}

object Board {

  private object OffsetConstants {
    val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
    val (diagonals, verticalHorizontal) = (Vector(upRight, downRight, downLeft, upLeft), Vector(up, left, down, right))
    val adjacent = diagonals ++ verticalHorizontal
    val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
  }

  private val blackSquares: Set[Square] = Square.Squares.flatten.toSet.filter(s => s.file % 2 == 0 && s.file % 2 == 0)

  private case class CastleConstants(move: Castle, rookStart: Square, kingEnd: Square, between: Set[Square], through: Set[Square], rookEnd: Square)
  private object CastleConstants {
    val whiteQueens = CastleConstants(`O-O-O`, A1, C1, Set(B1, C1, D1), Set(C1, D1), D1)
    val whiteKings = CastleConstants(`O-O`, H1, G1, Set(F1, G1), Set(F1), F1)
    val blackQueens = CastleConstants(`O-O-O`, A8, C8, Set(B8, C8, D8), Set(C8, D8), D8)
    val blackKings = CastleConstants(`O-O`, H8, G8, Set(F8, G8), Set(F8), F8)
  }

  def initial = {
    val same = Side(White, rank(_2), Set(B1, G1), Set(C1, F1), Set(A1, H1), Set(D1), Set(E1))
    val opponent = Side(Black, rank(_7), Set(B8, G8), Set(C8, F8), Set(A8, H8), Set(D8), Set(E8))
    val h = calculateHash(same, opponent, None)
    val b = Board(same, opponent, None, Vector.empty[(Side, Side)], 0, h, Nil, 0)
    b.copy(evalScore = Simplified.calculateFullScore(b))
  }

  def calculateHash(same: Side, opponent: Side, twoSquarePawn: Option[Move]): Long = {
    var h = 0L
    def addSide(s: Side): Unit = {
      val cIdx = Zobrist.getColorIndex(s.color)
      s.pawns.foreach(sq => h ^= Zobrist.pieces(cIdx)(Zobrist.getPieceIndex(Pawn))(Zobrist.getSquareIndex(sq)))
      s.knights.foreach(sq => h ^= Zobrist.pieces(cIdx)(Zobrist.getPieceIndex(Knight))(Zobrist.getSquareIndex(sq)))
      s.bishops.foreach(sq => h ^= Zobrist.pieces(cIdx)(Zobrist.getPieceIndex(Bishop))(Zobrist.getSquareIndex(sq)))
      s.rooks.foreach(sq => h ^= Zobrist.pieces(cIdx)(Zobrist.getPieceIndex(Rook))(Zobrist.getSquareIndex(sq)))
      s.queens.foreach(sq => h ^= Zobrist.pieces(cIdx)(Zobrist.getPieceIndex(Queen))(Zobrist.getSquareIndex(sq)))
      s.kings.foreach(sq => h ^= Zobrist.pieces(cIdx)(Zobrist.getPieceIndex(King))(Zobrist.getSquareIndex(sq)))
    }
    addSide(same)
    addSide(opponent)

    if (same.color == Black) h ^= Zobrist.sideToMove

    // Castling
    val white = if (same.color == White) same else opponent
    val black = if (same.color == Black) same else opponent

    if (white.color == White) {
      if (white.kings.contains(E1) && !white.moved.contains(E1)) {
        if (white.rooks.contains(H1) && !white.moved.contains(H1)) h ^= Zobrist.castling(0)
        if (white.rooks.contains(A1) && !white.moved.contains(A1)) h ^= Zobrist.castling(1)
      }
    }
    if (black.color == Black) {
      if (black.kings.contains(E8) && !black.moved.contains(E8)) {
        if (black.rooks.contains(H8) && !black.moved.contains(H8)) h ^= Zobrist.castling(2)
        if (black.rooks.contains(A8) && !black.moved.contains(A8)) h ^= Zobrist.castling(3)
      }
    }

    twoSquarePawn.foreach(m => h ^= Zobrist.enPassant(m.end.file))

    h
  }

  private def isCheck(defender: Side, offender: Side): Boolean = {
    import OffsetConstants._

    def rq(s: Square) = offender.rooks.contains(s) || offender.queens.contains(s)
    def rqk(s: Square) = rq(s) || offender.kings.contains(s)
    def bq(s: Square) = offender.bishops.contains(s) || offender.queens.contains(s)
    def bqk(s: Square) = bq(s) || offender.kings.contains(s)
    def pbqk(s: Square) = bqk(s) || offender.pawns.contains(s)

    defender.kings.exists { king =>

      def givingCheck(inOneStep: Square => Boolean, inAnySteps: Square => Boolean)(offset: (Int, Int)): Boolean = {
        var c = king + offset
        var g = inOneStep
        while (c != Outside && !defender.contains(c)) {
          if (g(c)) return true
          if (offender.contains(c)) return false
          c = c + offset
          g = inAnySteps
        }
        false
      }

      verticalHorizontal.exists(givingCheck(rqk, rq)) ||
        knight.exists(offset => offender.knights.contains(king + offset)) ||
        Vector(downLeft, downRight).exists(givingCheck(if (defender.color == Black) pbqk else bqk, bq)) ||
        Vector(upLeft, upRight).exists(givingCheck(if (defender.color == White) pbqk else bqk, bq))

    }

  }

}
import Board._

case class Board private(same: Side, opponent: Side, private val twoSquarePawn: Option[Move], private val positions: Vector[(Side, Side)], private val pawnOrCapture: Int, val hash: Long, val history: List[Long], val evalScore: Int) {

  def leaves: Vector[(MoveBase, Board)] = {
    import OffsetConstants._
    import CastleConstants._
    import mateinone.evaluators.Simplified

    // Functions that generate ends from the specified start and offset and filters illegal ends
    def oneStep(start: Square, offset: (Int, Int), acceptEnd: Square => Boolean): Set[Square] =
      { val e = start + offset; if (e != Outside && acceptEnd(e)) Set(e) else Set.empty[Square] }
    val openOnly1 = oneStep(_: Square, _: (Int, Int), e => !same.contains(e) && !opponent.contains(e))
    def openOnly2(start: Square, offset: (Int, Int)): Set[Square] = openOnly1(start, offset).flatMap(openOnly1(_, offset))
    val captureOnly = oneStep(_: Square, _: (Int, Int), opponent.contains)
    val openOrCapture = oneStep(_: Square, _: (Int, Int), !same.contains(_))
    def openUntilCapture(start: Square, offset: (Int, Int)): Set[Square] = {
      var result = Set.empty[Square]; var e = start + offset
      while (e != Outside && !same.contains(e) && !opponent.contains(e)) { result = result + e; e = e + offset }
      if (e != Outside && opponent.contains(e)) result + e else result
    }

    // Functions that generate moves from start and ends squares
    def toMoves(start: Square, ends: Set[Square]): Set[Move] = ends.map(Move.move(start, _))
    def toPromotions(start: Square, ends: Set[Square]): Set[Promotion] = ends.flatMap(e => PromotionType.all.map(t => Promotion(start, e, t)))

    // Functions that generate a board from the current board and the specified move
    def doMove(`type`: PieceType, isTwoSquare: Boolean = false)(m: Move): Board = {
      val isCapture = opponent.contains(m.end)
      val victimTypeOpt = if (isCapture) opponent.typeAt(m.end) else None
      
      var nextEvalScore = evalScore
      // Remove piece from start
      nextEvalScore -= Simplified.pieceValue(`type`, same.color, m.start, false)
      // Add piece to end
      nextEvalScore += Simplified.pieceValue(`type`, same.color, m.end, false)
      // If capture, remove victim
      victimTypeOpt.foreach { vt =>
        nextEvalScore -= Simplified.pieceValue(vt, opponent.color, m.end, false)
      }

      val nextOpponent = if (isCapture) opponent.remove(m.end) else opponent
      val nextSame = same.add(m.end, `type`).remove(m.start)
      val nextTwoSquare = if (isTwoSquare) Some(m) else None
      val nextPositions = if (`type` == Pawn || isCapture) Vector.empty[(Side, Side)] else positions :+ (same, opponent)
      val nextPawnOrCapture = if (`type` == Pawn || isCapture) 0 else pawnOrCapture + 1
      val nextHash = Board.calculateHash(nextOpponent, nextSame, nextTwoSquare)
      Board(nextOpponent, nextSame, nextTwoSquare, nextPositions, nextPawnOrCapture, nextHash, hash :: history, nextEvalScore)
    }
    def doPromotion(p: Promotion): Board = {
      val isCapture = opponent.contains(p.end)
      val victimTypeOpt = if (isCapture) opponent.typeAt(p.end) else None
      
      var nextEvalScore = evalScore
      // Remove pawn from start
      nextEvalScore -= Simplified.pieceValue(Pawn, same.color, p.start, false)
      // Add promoted piece to end
      nextEvalScore += Simplified.pieceValue(p.`type`, same.color, p.end, false)
      // If capture, remove victim
      victimTypeOpt.foreach { vt =>
        nextEvalScore -= Simplified.pieceValue(vt, opponent.color, p.end, false)
      }

      val nextOpponent = if (isCapture) opponent.remove(p.end) else opponent
      val nextSame = same.remove(p.start).add(p.end, p.`type`)
      val nextHash = Board.calculateHash(nextOpponent, nextSame, None)
      Board(nextOpponent, nextSame, None, Vector.empty[(Side, Side)], 0, nextHash, hash :: history, nextEvalScore)
    }

    def createLeaves[M <: MoveBase](starts: Set[Square],
                                    offsets: Vector[(Int, Int)],
                                    createEnds: (Square, (Int, Int)) => Set[Square],
                                    createMoves: (Square, Set[Square]) => Set[M],
                                    createBoard: M => Board): Vector[(M, Board)] =
      starts.toVector.flatMap(s => offsets.flatMap(o => createMoves(s, createEnds(s, o))).map(m => (m, createBoard(m))))

    (createLeaves(same.knights, knight, openOrCapture, toMoves, doMove(Knight)) ++
      createLeaves(same.bishops, diagonals, openUntilCapture, toMoves, doMove(Bishop)) ++
      createLeaves(same.rooks, verticalHorizontal, openUntilCapture, toMoves, doMove(Rook)) ++
      createLeaves(same.queens, adjacent, openUntilCapture, toMoves, doMove(Queen)) ++
      createLeaves(same.kings, adjacent, openOrCapture, toMoves, doMove(King)) ++
      {
        val (promotionRank, advance, captures) = if (same.color == White) (rank(_7), Vector(up), Vector(upLeft, upRight)) else (rank(_2), Vector(down), Vector(downLeft, downRight))
        val (promoters, movers) = same.pawns.partition(promotionRank.contains)
        createLeaves(promoters, advance, openOnly1, toPromotions, doPromotion) ++
          createLeaves(promoters, captures, captureOnly, toPromotions, doPromotion) ++
          createLeaves(movers, captures, openOnly1, toMoves, doMove(Pawn))
            .filter { case (m, b) => twoSquarePawn.exists(l => l.end.file == m.end.file && l.end.rank == m.start.rank) }
            .map { case (m, b) =>
              val capturedPawnSq = twoSquarePawn.get.end
              val finalSame = b.same.remove(capturedPawnSq)
              val nextHash = Board.calculateHash(b.opponent, finalSame, None)
              // Update eval for En Passant capture
              val nextEvalScore = b.evalScore - Simplified.pieceValue(Pawn, opponent.color, capturedPawnSq, false)
              (m, b.copy(same = finalSame, hash = nextHash, evalScore = nextEvalScore))
            } ++
          createLeaves(movers.filterNot(same.moved), advance, openOnly2, toMoves, doMove(Pawn, isTwoSquare = true)) ++
          createLeaves(movers, advance, openOnly1, toMoves, doMove(Pawn)) ++
          createLeaves(movers, captures, captureOnly, toMoves, doMove(Pawn))
      } ++
      {
        val (kingStart, kingside, queenside) = if (same.color == White) (E1, whiteKings, whiteQueens) else (E8, blackKings, blackQueens)
        var castleLeaves = Vector.empty[(Castle, Board)]
        if (same.kings.contains(kingStart) && !same.moved.contains(kingStart) && !isCheck) {
          def createLeaf(cc: CastleConstants): Unit = {
            def betweenOccupied = cc.between.exists(b => same.contains(b) || opponent.contains(b))
            def throughCheck = Board.isCheck(cc.through.foldLeft(same)(_.add(_, King)), opponent)
            def board = {
              val nextOpponent = same.remove(cc.rookStart).remove(kingStart).add(cc.rookEnd, Rook).add(cc.kingEnd, King)
              val nextSame = opponent
              val nextHash = Board.calculateHash(nextSame, nextOpponent, None)
              
              var nextEvalScore = evalScore
              // Update eval for Castle (King and Rook move)
              nextEvalScore -= Simplified.pieceValue(King, same.color, kingStart, false)
              nextEvalScore -= Simplified.pieceValue(Rook, same.color, cc.rookStart, false)
              nextEvalScore += Simplified.pieceValue(King, same.color, cc.kingEnd, false)
              nextEvalScore += Simplified.pieceValue(Rook, same.color, cc.rookEnd, false)
              
              Board(nextSame, nextOpponent, None, Vector.empty[(Side, Side)], pawnOrCapture + 1, nextHash, hash :: history, nextEvalScore)
            }
            if (same.rooks.contains(cc.rookStart) && !same.moved.contains(cc.rookStart) && !betweenOccupied && !throughCheck) castleLeaves = castleLeaves :+(cc.move, board)
          }
          createLeaf(kingside); createLeaf(queenside)
        }
        castleLeaves
      }).filterNot { case (_, b) => Board.isCheck(b.opponent, b.same) }

  }

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

  def isCheck: Boolean = Board.isCheck(same, opponent)
  def isCheckmate: Boolean = moves.isEmpty && isCheck

  def isStalemate: Boolean = moves.isEmpty && !isCheck
  def isInsufficientMaterial: Boolean = {
    import Board.blackSquares
    def bothPieces(f: Side => Set[Square]) = f(same) ++ f(opponent)
    val numKnights = bothPieces(_.knights).size; val bishops = bothPieces(_.bishops)
    (bothPieces(_.pawns).isEmpty && bothPieces(_.rooks).isEmpty && bothPieces(_.queens).isEmpty) &&
      ((numKnights == 0 || bishops.isEmpty) ||
        (numKnights == 1 && bishops.isEmpty) ||
        (numKnights == 0 && (bishops.forall(blackSquares.contains) || !bishops.exists(blackSquares.contains))))
  }
  def isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  def isThreefoldRepetition: Boolean = history.count(_ == hash) >= 2
  def isFiftyMoveRule: Boolean = pawnOrCapture >= 100
  def mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] =
    movesToMake match { case h :: t => leaves.find(_._1 == h).flatMap(_._2.move(t)); case _ => Some(this) }
  def move(movesToMake: MoveBase*): Option[Board] =
    move(movesToMake.toList)

}
