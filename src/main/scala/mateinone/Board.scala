package mateinone

import Square._

object Board {

  private val blackSquares: Set[Square] = Square.Squares.flatten.toSet.filter(s => s.file % 2 == 0 && s.file % 2 == 0)

  def initial: Board = {
    val same = Pieces(Set(A2, B2, C2, D2, E2, F2, G2, H2), Set(B1, G1), Set(C1, F1), Set(A1, H1), Set(D1), Set(E1))
    val opponent = Pieces(Set(A7, B7, C7, D7, E7, F7, G7, H7), Set(B8, G8), Set(C8, F8), Set(A8, H8), Set(D8), Set(E8))
    val open = Set(A3, A4, A5, A6, B3, B4, B5, B6, C3, C4, C5, C6, D3, D4, D5, D6, E3, E4, E5, E6, F3, F4, F5, F6, G3, G4, G5, G6, H3, H4, H5, H6)
    Board(White, same, opponent, open, Set.empty[Square])
  }

  private val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
  private val (whitePawnAdvance, blackPawnAdvance) = (Vector(up), Vector(down))
  private val (whitePawnCaptures, blackPawnCaptures) = (Vector(upLeft, upRight), Vector(downLeft, downRight))
  private val diagonals = Vector(upRight, downRight, downLeft, upLeft)
  private val rook = Vector(up, left, down, right)
  private val adjacent = diagonals ++ rook
  private val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
  private val (whitePromotionRank, blackPromotionRank) = (Set(A7, B7, C7, D7, E7, F7, G7, H7), Set(A2, B2, C2, D2, E2, F2, G2, H2))

  private def isCheck(defender: Side, defenderKings: Set[Square], offender: Pieces, open: Set[Square]): Boolean =
    defenderKings.exists { king =>
      def givingCheck(inOneStep: Square => Boolean, inAnySteps: Square => Boolean)(offset: (Int, Int)): Boolean = {
        var current = king + offset; var isGivingCheck = inOneStep
        while (open.contains(current)) { current = current + offset; isGivingCheck = inAnySteps }
        isGivingCheck(current)
      }
      def q(s: Square): Boolean = offender.queens.contains(s)
      def k(s: Square): Boolean = offender.kings.contains(s)
      def rq(s: Square): Boolean = offender.rooks.contains(s) || q(s)
      def rqk(s: Square): Boolean = rq(s) || k(s)
      def bq(s: Square): Boolean = offender.bishops.contains(s) || q(s)
      def bqk(s: Square): Boolean = bq(s) || k(s)
      def pbqk(s: Square): Boolean = offender.pawns.contains(s) || bqk(s)
      rook.exists(givingCheck(rqk, rq)) ||
        knight.exists(offset => offender.knights.contains(king + offset)) ||
        blackPawnCaptures.exists(givingCheck(if (defender == Black) pbqk else bqk, bq)) ||
        whitePawnCaptures.exists(givingCheck(if (defender == White) pbqk else bqk, bq))
    }

}
import Board._

case class Pieces(pawns: Set[Square], knights: Set[Square], bishops: Set[Square], rooks: Set[Square], queens: Set[Square], kings: Set[Square]) {
  def typeOf(s: Square): Option[PieceType] = if (pawns.contains(s)) Some(Pawn) else if (knights.contains(s)) Some(Knight)
    else if (bishops.contains(s)) Some(Bishop) else if (rooks.contains(s)) Some(Rook) else if (queens.contains(s)) Some(Queen)
    else if (kings.contains(s)) Some(King) else None
  def add(s: Square, t: PieceType): Pieces = t match {
    case Pawn => copy(pawns = pawns + s); case Knight => copy(knights = knights + s); case Bishop => copy(bishops = bishops + s);
    case Rook => copy(rooks = rooks + s); case Queen => copy(queens = queens + s); case King => copy(kings = kings + s) }
  def move(s: Square, e: Square): Pieces = typeOf(s) match {
    case Some(Pawn) => copy(pawns = pawns - s + e); case Some(Knight) => copy(knights = knights - s + e); case Some(Bishop) => copy(bishops = bishops - s + e);
    case Some(Rook) => copy(rooks = rooks - s + e); case Some(Queen) => copy(queens = queens - s + e); case Some(King) => copy(kings = kings - s + e) }
  def remove(s: Square): Pieces = typeOf(s) match {
    case Some(Pawn) => copy(pawns = pawns - s); case Some(Knight) => copy(knights = knights - s); case Some(Bishop) => copy(bishops = bishops - s);
    case Some(Rook) => copy(rooks = rooks - s); case Some(Queen) => copy(queens = queens - s); case Some(King) => copy(kings = kings - s); case None => this }
  def contains(s: Square, t: PieceType): Boolean = t match {
    case Pawn => pawns.contains(s); case Knight => knights.contains(s); case Bishop => bishops.contains(s);
    case Rook => rooks.contains(s); case Queen => queens.contains(s); case King => kings.contains(s) }
  def contains(s: Square): Boolean = typeOf(s).isDefined
  override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}

case class Board private(turn: Side, same: Pieces, opponent: Pieces, open: Set[Square], moved: Set[Square]) {

  private def ofType(fn: Pieces => Set[Square]): Set[Square] = fn(same) ++ fn(opponent)

  def pieces: Set[(Side, PieceType, Square)] = {
    def toPieces(side: Side, squares: Set[Square], `type`: PieceType) = squares.map((side, `type`, _))
    toPieces(turn, same.pawns, Pawn) ++ toPieces(turn.other, opponent.pawns, Pawn) ++
      toPieces(turn, same.knights, Knight) ++ toPieces(turn.other, opponent.knights, Knight) ++
      toPieces(turn, same.bishops, Bishop) ++ toPieces(turn.other, opponent.bishops, Bishop) ++
      toPieces(turn, same.rooks, Rook) ++ toPieces(turn.other, opponent.rooks, Rook) ++
      toPieces(turn, same.queens, Queen) ++ toPieces(turn.other, opponent.queens, Queen) ++
      toPieces(turn, same.kings, King) ++ toPieces(turn.other, opponent.kings, King)
  }

  lazy val leaves: Vector[(MoveBase, Board)] = {

    def doMove(m: Move): Board =
      Board(turn.other, opponent.remove(m.end), same.move(m.start, m.end), open + m.start - m.end, moved + m.end)

    def doPromotion(p: Promotion): Board =
      Board(turn.other, opponent.remove(p.end), same.remove(p.start).add(p.end, p.`type`), open + p.start - p.end, moved)

    def createLeaves[M <: MoveBase](starts: Set[Square],
                                    offsets: Vector[(Int, Int)],
                                    createEnds: (Square, (Int, Int)) => Set[Square],
                                    createMoves: (Square, Square) => Vector[M],
                                    createBoard: M => Board): Vector[(M, Board)] = {
      def placesKingInCheck(b : Board) = Board.isCheck(b.turn.other, b.opponent.kings, b.same, b.open)
      starts.toVector.flatMap(s => offsets.flatMap(createEnds(s, _)).flatMap(e => createMoves(s, e)).map(m => (m, createBoard(m)))).filterNot(l => placesKingInCheck(l._2))
    }

    def openOrCapture(start: Square, offset: (Int, Int)): Set[Square] = {
      val end = start + offset
      if (open.contains(end) || opponent.contains(end)) Set(end) else Set.empty[Square]
    }

    def openUntilCapture(start: Square, offset: (Int, Int)): Set[Square] = {
      var result = Set.empty[Square]
      var current = start + offset
      while (open.contains(current)) {
        result = result + current; current = current + offset
      }
      if (opponent.contains(current)) result + current else result
    }

    def openToOccupied1(start: Square, offset: (Int, Int)): Set[Square] = {
      val end = start + offset
      if (open.contains(end)) Set(end) else Set.empty[Square]
    }

    def openToOccupied2(start: Square, offset: (Int, Int)): Set[Square] = {
      val ones = openToOccupied1(start, offset)
      ones ++ ones.flatMap(openToOccupied1(_, offset))
    }

    def captureOnly(start: Square, offset: (Int, Int)): Set[Square] = {
      val end = start + offset
      if (opponent.contains(end)) Set(end) else Set.empty[Square]
    }

    def toMoves(start: Square, end: Square): Vector[Move] = Vector(Move.move(start, end))

    def toPromotions(start: Square, end: Square): Vector[Promotion] = PromotionType.all.map(t => Promotion(start, end, t))

    val pawnLeaves = {
      val promotionRank = if (turn == White) whitePromotionRank else blackPromotionRank
      val advance = if (turn == White) whitePawnAdvance else blackPawnAdvance
      val captures = if (turn == White) whitePawnCaptures else blackPawnCaptures
      createLeaves(same.pawns & promotionRank, advance, openToOccupied1, toPromotions, doPromotion) ++
        createLeaves(same.pawns & promotionRank, captures, captureOnly, toPromotions, doPromotion) ++
        createLeaves(same.pawns &~ moved, advance, openToOccupied2, toMoves, doMove) ++
        createLeaves((same.pawns & moved) &~ promotionRank, advance, openToOccupied1, toMoves, doMove) ++
        createLeaves(same.pawns &~ promotionRank, captures, captureOnly, toMoves, doMove)
    }

    val castleLeaves: Vector[(Castle, Board)] = {

      val castleMoves = {
        def canCastle(rookStart: Square, between: Set[Square]): Boolean = !moved.contains(E1) && !moved.contains(rookStart) && between.forall(open.contains)
        val kingside = if (turn == White) canCastle(H1, Set(F1, G1)) else canCastle(H8, Set(F8, G8))
        val queenside = if (turn == White) canCastle(A1, Set(B1, C1, D1)) else canCastle(A8, Set(B8, C8, D8))
        if (kingside && queenside) Vector(`O-O`, `O-O-O`) else if (kingside) Vector(`O-O`) else if (queenside) Vector(`O-O-O`) else Vector.empty[Castle]
      }

      def castleBoard(kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square) =
        Board(turn.other, opponent, same.copy(rooks = same.rooks - rookStart + rookEnd, kings = same.kings - kingStart + kingEnd),
          open + kingStart + rookStart - kingEnd - rookEnd, moved + kingEnd + rookEnd)

      def castlesThroughCheck(between: Set[Square]): Boolean = Board.isCheck(turn, same.kings ++ between, opponent, open &~ between)

      castleMoves
        .filter {
          case `O-O` => !castlesThroughCheck(if (turn == White) Set(F1) else Set(F8))
          case `O-O-O` => !castlesThroughCheck(if (turn == White) Set(C1, D1) else Set(C8, D8)) }
        .map {
          case `O-O` if turn == White => `O-O` -> castleBoard(E1, G1, H1, F1)
          case `O-O` if turn == Black => `O-O` -> castleBoard(E8, G8, H8, F8)
          case `O-O-O` if turn == White => `O-O-O` -> castleBoard(E1, C1, A1, D1)
          case `O-O-O` if turn == Black => `O-O-O` -> castleBoard(E8, C8, A8, D8) }

    }

    createLeaves(same.knights, knight, openOrCapture, toMoves, doMove) ++
      createLeaves(same.bishops, diagonals, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.rooks, rook, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.queens, adjacent, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.kings, adjacent, openOrCapture, toMoves, doMove) ++ pawnLeaves ++ castleLeaves

  }

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

  lazy val isCheck: Boolean = Board.isCheck(turn, same.kings, opponent, open)
  lazy val isCheckmate: Boolean = moves.isEmpty && isCheck

  lazy val isStalemate: Boolean = moves.isEmpty && !isCheck
  lazy val isInsufficientMaterial: Boolean = {
    val (knights, bishops) = (ofType(_.knights), ofType(_.bishops))
    (ofType(_.pawns).isEmpty || ofType(_.rooks).isEmpty || ofType(_.queens).isEmpty) &&
      ((knights.isEmpty || bishops.isEmpty) || (knights.size == 1 && bishops.isEmpty) ||
       (knights.isEmpty && (bishops.forall(blackSquares.contains) || !bishops.exists(blackSquares.contains))))
  }
  lazy val isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  lazy val isThreefoldRepetition: Boolean = false
  lazy val isFiftyMoveRule: Boolean = false
  lazy val mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match { case h :: t => leaves.find(_._1 == h).flatMap(_._2.move(t)); case _ => Some(this) }
  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString: String = "Board("+turn+","+pieces+")"

}
