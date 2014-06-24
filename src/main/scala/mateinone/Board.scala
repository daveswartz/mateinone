package mateinone

import Square._
import scala.collection.mutable

object Board {

  def initial: Board = {
    val same = Set(A1, A2, B1, B2, C1, C2, D1, D2, E1, E2, F1, F2, G1, G2, H1, H2)
    val opponent = Set(A8, A7, B8, B7, C8, C7, D8, D7, E8, E7, F8, F7, G8, G7, H8, H7)
    val open = Set(A3, A4, A5, A6, B3, B4, B5, B6, C3, C4, C5, C6, D3, D4, D5, D6, E3, E4, E5, E6, F3, F4, F5, F6, G3, G4, G5, G6, H3, H4, H5, H6)
    val pawns = Set(A2, B2, C2, D2, E2, F2, G2, H2, A7, B7, C7, D7, E7, F7, G7, H7)
    val (knights, bishops, rooks, queens, kings) = (Set(B1, G1, B8, G8), Set(C1, F1, C8, F8), Set(A1, H1, A8, H8), Set(D1, D8), Set(E1, E8))
    Board(White, same, opponent, open, Set(), pawns, knights, bishops, rooks, queens, kings, 0, None)
  }

  private val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
  private val whitePawnCaptures = Vector(upLeft, upRight)
  private val blackPawnCaptures = Vector(downLeft, downRight)
  private val adjacent = Vector(up, upRight, right, downRight, down, downLeft, left, upLeft)
  private val diagonals = Vector(upRight, downRight, downLeft, upLeft)
  private val rook = Vector(up, left, down, right)
  private val queen = rook ++ diagonals
  private val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))

  private def isCheck(defender: Side, defenderKings: Set[Square], open: Set[Square], pawns: Set[Square], knights: Set[Square],
                      bishops: Set[Square], rooks: Set[Square], queens: Set[Square], kings: Set[Square]): Boolean =
    defenderKings.exists { king =>
      def givingCheck(inOneStep: Set[Square], inAnySteps: Set[Square])(offset: (Int, Int)): Boolean = {
        var current = king + offset; var isGivingCheck = inOneStep
        while (open.contains(current)) { current = current + offset; isGivingCheck = inAnySteps }
        isGivingCheck.contains(current)
      }
      rook.exists(givingCheck(rooks | queens | kings, rooks | queens)) ||
        whitePawnCaptures.exists(givingCheck(if (defender == Black) pawns ++ bishops ++ queens ++ kings else bishops ++ queens ++ kings, bishops ++ queens)) ||
        blackPawnCaptures.exists(givingCheck(if (defender == White) pawns ++ bishops ++ queens ++ kings else bishops ++ queens ++ kings, bishops ++ queens)) ||
        knight.exists(offset => knights.contains(king + offset))
    }

}
import Board._

case class Board private(turn: Side, same: Set[Square], opponent: Set[Square], open: Set[Square], moved: Set[Square],
                         pawns: Set[Square], knights: Set[Square], bishops: Set[Square], rooks: Set[Square],
                         queens: Set[Square], kings: Set[Square], lastPawnMoveOrCapture: Int, twoSquarePawnAdvance: Option[Int]) {

  def pieces: Set[(Side, PieceType, Square)] = {
    def toPieces(ofType: Set[Square], `type`: PieceType) =
      (same & ofType).map((turn, `type`, _)) ++ (opponent & ofType).map((turn.other, `type`, _))
    toPieces(pawns, Pawn) ++ toPieces(knights, Knight) ++ toPieces(bishops, Bishop) ++ toPieces(rooks, Rook) ++
      toPieces(queens, Queen) ++ toPieces(kings, King)
  }

  lazy val leaves: Vector[(MoveBase, Board)] = {

    def doMove(m: Move): Board = {
      def moveIfPresent(s: Set[Square]): Set[Square] = if (s.contains(m.start)) s - m.start + m.end else s
      val last = if ((opponent | pawns).contains(m.end)) 0 else lastPawnMoveOrCapture + 1
      val two = if (pawns.contains(m.start) && math.abs(m.end.rank - m.start.rank) == 2) Some(m.end.file) else None
      Board(turn.other, opponent - m.end, same - m.start + m.end, open + m.start - m.end, moved + m.end,
        moveIfPresent(pawns), moveIfPresent(knights), moveIfPresent(bishops), moveIfPresent(rooks),
        moveIfPresent(queens), moveIfPresent(kings), last, two)
    }

    def doPromotion(p: Promotion): Board = {
      def moveIfType(t: PieceType, s: Set[Square]): Set[Square] = if (p.`type` == t) s - p.start + p.end else s
      Board(turn.other, opponent - p.end, same - p.start + p.end, open + p.start - p.end, moved, pawns - p.start,
        moveIfType(Knight, knights), moveIfType(Bishop, bishops), moveIfType(Rook, rooks), moveIfType(Queen, queens),
        moveIfType(King, kings), 0, None)
    }

    def createLeaves[M <: MoveBase](starts: Set[Square],
                                    offsets: Vector[(Int, Int)],
                                    createEnds: (Square, (Int, Int)) => Vector[Square],
                                    createMoves: (Square, Square) => Vector[M],
                                    createBoard: M => Board): Set[(M, Board)] = {
      def placesKingInCheck(b : Board) = Board.isCheck(b.turn.other, b.opponent & b.kings, b.open, b.same & b.pawns,
        b.same &  b.knights, b.same & b.bishops, b.same & b.rooks, b.same & b.queens, b.same & b.kings)
      starts.flatMap(s => offsets.flatMap(createEnds(s, _)).flatMap(e => createMoves(s, e)).map(m => (m, createBoard(m)))).filterNot(l => placesKingInCheck(l._2))
    }

    def openOrCapture(start: Square, offset: (Int, Int)): Vector[Square] = {
      val end = start + offset
      if (open.contains(end) || opponent.contains(end)) Vector(end) else Vector.empty[Square]
    }

    def openUntilCapture(start: Square, offset: (Int, Int)): Vector[Square] = {
      val result = new mutable.Stack[Square]
      var current = start + offset
      while (open.contains(current)) {
        result.push(current); current = current + offset
      }
      (if (opponent.contains(current)) result.push(current) else result).toVector
    }

    def openToOccupied1(start: Square, offset: (Int, Int)): Vector[Square] = {
      val end = start + offset
      if (open.contains(end)) Vector(end) else Vector.empty[Square]
    }

    def openToOccupied2(start: Square, offset: (Int, Int)): Vector[Square] = {
      val ones = openToOccupied1(start, offset)
      ones ++ ones.flatMap(openToOccupied1(_, offset))
    }

    def captureOnly(start: Square, offset: (Int, Int)): Vector[Square] = {
      val end = start + offset
      if (opponent.contains(end)) Vector(end) else Vector.empty[Square]
    }

    def toMoves(start: Square, end: Square): Vector[Move] = Vector(Move.move(start, end))

    def toPromotions(start: Square, end: Square): Vector[Promotion] = PromotionType.all.map(t => Promotion(start, end, t))

    val pawnLeaves = {
      val promotionRank = if (turn == White) Set(A7, B7, C7, D7, E7, F7, G7, H7) else Set(A2, B2, C2, D2, E2, F2, G2, H2)
      val advance = if (turn == White) Vector(up) else Vector(down)
      val captures = if (turn == White) whitePawnCaptures else blackPawnCaptures
      createLeaves(same & pawns & promotionRank, advance, openToOccupied1, toPromotions, doPromotion) ++
        createLeaves(same & pawns & promotionRank, captures, captureOnly, toPromotions, doPromotion) ++
        createLeaves((same & pawns) &~ moved, advance, openToOccupied2, toMoves, doMove) ++
        createLeaves((same & pawns & moved) &~ promotionRank, advance, openToOccupied1, toMoves, doMove) ++
        createLeaves((same & pawns) &~ promotionRank, captures, captureOnly, toMoves, doMove)
    }

    val castleLeaves: Set[(Castle, Board)] = {

      val castleMoves = {
        def canCastle(rookStart: Square, between: Set[Square]): Boolean = !moved.contains(E1) && !moved.contains(rookStart) && between.forall(open.contains)
        val kingside = if (turn == White) canCastle(H1, Set(F1, G1)) else canCastle(H8, Set(F8, G8))
        val queenside = if (turn == White) canCastle(A1, Set(B1, C1, D1)) else canCastle(A8, Set(B8, C8, D8))
        if (kingside && queenside) Set(`O-O`, `O-O-O`) else if (kingside) Set(`O-O`) else if (queenside) Set(`O-O-O`) else Set()
      }

      def castleBoard(kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square) =
        Board(turn.other, opponent, same - kingStart - rookStart + kingEnd + rookEnd,
          open + kingStart + rookStart - kingEnd - rookEnd, moved + kingEnd + rookEnd, pawns, knights, bishops,
          rooks - rookStart + rookEnd, queens, kings - kingStart + kingEnd, lastPawnMoveOrCapture + 1, None)

      def castlesThroughCheck(between: Set[Square]): Boolean =
        Board.isCheck(turn, (same & kings) ++ between, open &~ between, opponent & pawns, opponent &  knights,
          opponent & bishops, opponent & rooks, opponent & queens, opponent & kings)

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

    (createLeaves(same & knights, knight, openOrCapture, toMoves, doMove) ++
      createLeaves(same & bishops, diagonals, openUntilCapture, toMoves, doMove) ++
      createLeaves(same & rooks, rook, openUntilCapture, toMoves, doMove) ++
      createLeaves(same & queens, queen, openUntilCapture, toMoves, doMove) ++
      createLeaves(same & kings, adjacent, openOrCapture, toMoves, doMove) ++ pawnLeaves ++ castleLeaves).toVector

  }

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

  def isCheck: Boolean = Board.isCheck(turn, same & kings, open, opponent & pawns, opponent &  knights,
    opponent & bishops, opponent & rooks, opponent & queens, opponent & kings)
  def isCheckmate: Boolean = moves.isEmpty && isCheck

  def isStalemate: Boolean = moves.isEmpty && !isCheck
  def isInsufficientMaterial: Boolean = {
    def isBlack(s: Square) = s.file % 2 == 0 && s.file % 2 == 0
    pawns.isEmpty && rooks.isEmpty && queens.isEmpty &&
      ((knights.isEmpty && bishops.isEmpty) || (knights.size == 1 && bishops.isEmpty) ||
        (knights.isEmpty && (bishops.forall(isBlack) || bishops.forall(!isBlack(_)))))
  }
  def isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  def isThreefoldRepetition: Boolean = false
  def isFiftyMoveRule: Boolean = lastPawnMoveOrCapture >= 100
  def mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match { case h :: t => leaves.find(_._1 == h).flatMap(_._2.move(t)); case _ => Some(this) }
  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString: String = "Board("+turn+","+pieces+")"

}
