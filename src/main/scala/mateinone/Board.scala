package mateinone

import Rank._
import Square._

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

  def initial = Board(Side(White, rank(_2), Set(B1, G1), Set(C1, F1), Set(A1, H1), Set(D1), Set(E1)), Side(Black, rank(_7), Set(B8, G8), Set(C8, F8), Set(A8, H8), Set(D8), Set(E8)), None, Vector.empty[(Side, Side)], 0)

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

case class Board private(same: Side, opponent: Side, private val twoSquarePawn: Option[Move], private val positions: Vector[(Side, Side)], private val pawnOrCapture: Int) {

  def leaves: Vector[(MoveBase, Board)] = {
    import OffsetConstants._
    import CastleConstants._

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
      Board(if (isCapture) opponent.remove(m.end) else opponent,
            same.add(m.end, `type`).remove(m.start),
            if (isTwoSquare) Some(m) else None,
            if (`type` == Pawn || isCapture) Vector.empty[(Side, Side)] else positions :+ (same, opponent),
            if (`type` == Pawn || isCapture) 0 else pawnOrCapture + 1)
    }
    def doPromotion(p: Promotion): Board = Board(if (opponent.contains(p.end)) opponent.remove(p.end) else opponent, same.remove(p.start).add(p.end, p.`type`), None, Vector.empty[(Side, Side)], 0)

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
            .map { case (m, b) => (m, b.copy(same = b.same.remove(twoSquarePawn.get.end))) } ++
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
            def board = Board(opponent, same.remove(cc.rookStart).remove(kingStart).add(cc.rookEnd, Rook).add(cc.kingEnd, King), None, Vector.empty[(Side, Side)], pawnOrCapture + 1)
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
    def both(f: Side => Set[Square]) = f(same) ++ f(opponent)
    val numKnights = both(_.knights).size; val bishops = both(_.bishops)
    (both(_.pawns).isEmpty && both(_.rooks).isEmpty && both(_.queens).isEmpty) &&
      ((numKnights == 0 || bishops.isEmpty) ||
        (numKnights == 1 && bishops.isEmpty) ||
        (numKnights == 0 && (bishops.forall(blackSquares.contains) || !bishops.exists(blackSquares.contains))))
  }
  def isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  def isThreefoldRepetition: Boolean = {
    def position(s: Side) =
      (s.kings, s.moved.intersect(s.kings), s.rooks, s.moved.intersect(s.rooks),
        s.pawns, s.knights, s.bishops, s.queens)
    val samePosition = position(same)
    val opponentPosition = position(opponent)
    positions.count { case (s, o) => position(s) == samePosition && position(o) == opponentPosition } == 2
  }
  def isFiftyMoveRule: Boolean = pawnOrCapture >= 100
  def mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] =
    movesToMake match { case h :: t => leaves.find(_._1 == h).flatMap(_._2.move(t)); case _ => Some(this) }
  def move(movesToMake: MoveBase*): Option[Board] =
    move(movesToMake.toList)

}
