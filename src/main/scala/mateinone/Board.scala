package mateinone

import Rank._
import Square._

object Side {
  def apply(color: Color, pawns: Set[Square], knights: Set[Square], bishops: Set[Square], rooks: Set[Square], queens: Set[Square], kings: Set[Square]): Side =
    Side(color, (pawns.map(_ -> Pawn) ++ knights.map(_ -> Knight) ++ bishops.map(_ -> Bishop) ++ rooks.map(_ -> Rook) ++ queens.map(_ -> Queen) ++ kings.map(_ -> King)).toMap,
      Map(Pawn -> pawns, Knight -> knights, Bishop -> bishops, Rook -> rooks, Queen -> queens, King -> kings), Set.empty[Square])
}

// Provides an interface to update and query a side's pieces (color, square, type and moved).
case class Side private(color: Color,
                        private val squareToType: Map[Square, PieceType],
                        private val typeToSquares: Map[PieceType, Set[Square]],
                        private val moved: Set[Square]) {
  def `type`(s: Square): PieceType = squareToType(s)
  def squares(t: PieceType): Set[Square] = typeToSquares(t)
  def moved(s: Square): Boolean = moved.contains(s)
  def contains(s: Square): Boolean = squareToType.contains(s)
  def contains(s: Square, t: PieceType*): Boolean = t.exists(typeToSquares(_).contains(s))
  def add(s: Square, t: PieceType): Side =
    copy(squareToType = squareToType + (s -> t), typeToSquares = typeToSquares + (t -> (typeToSquares(t) + s)), moved = moved + s)
  def remove(s: Square, t: PieceType): Side =
    copy(squareToType = squareToType - s, typeToSquares = typeToSquares + (t -> (typeToSquares(t) - s)))
}

object Board {

  private val blackSquares: Set[Square] = Square.Squares.flatten.toSet.filter(s => s.file % 2 == 0 && s.file % 2 == 0)

  def initial = Board(Side(White, rank(_2), Set(B1, G1), Set(C1, F1), Set(A1, H1), Set(D1), Set(E1)),
    Side(Black, rank(_7), Set(B8, G8), Set(C8, F8), Set(A8, H8), Set(D8), Set(E8)))

  private val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
  private val (upOnly, downOnly) = (Vector(up), Vector(down))
  private val (upLeftUpRight, downLeftDownRight) = (Vector(upLeft, upRight), Vector(downLeft, downRight))
  private val (diagonals, verticalHorizontal) = (Vector(upRight, downRight, downLeft, upLeft), Vector(up, left, down, right))
  private val adjacent = diagonals ++ verticalHorizontal
  private val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))

  private def isCheck(defender: Side, offender: Side): Boolean = {
    val rqk = offender.contains(_: Square, Rook, Queen, King)
    val rq = offender.contains(_: Square, Rook, Queen)
    val pbqk = offender.contains(_: Square, Pawn, Bishop, Queen, King)
    val bqk = offender.contains(_: Square, Bishop, Queen, King)
    val bq = offender.contains(_: Square, Bishop, Queen)
    val n = offender.contains(_: Square, Knight)
    defender.squares(King).exists { king =>
      def givingCheck(inOneStep: Square => Boolean, inAnySteps: Square => Boolean)(offset: (Int, Int)): Boolean = {
        var c = king + offset; var isGivingCheck = inOneStep
        while (c != Outside && !defender.contains(c) && !offender.contains(c)) { c = c + offset; isGivingCheck = inAnySteps }
        c != Outside && isGivingCheck(c)
      }
      verticalHorizontal.exists(givingCheck(rqk, rq)) || knight.exists(offset => n(king + offset)) ||
        downLeftDownRight.exists(givingCheck(if (defender.color == Black) pbqk else bqk, bq)) ||
        upLeftUpRight.exists(givingCheck(if (defender.color == White) pbqk else bqk, bq))
    }
  }

}
import Board._

case class Board private(same: Side, opponent: Side) {

  def leaves: Vector[(MoveBase, Board)] = {

    def oneStep(start: Square, offset: (Int, Int), filter: Square => Boolean): Set[Square] =
    { val end = start + offset; if (end != Outside && filter(end)) Set(end) else Set.empty[Square] }
    val openOnly1 = oneStep(_: Square, _: (Int, Int), s => !same.contains(s) && !opponent.contains(s))
    def openOnly2(start: Square, offset: (Int, Int)): Set[Square] =
    { val ones = openOnly1(start, offset); ones ++ ones.flatMap(openOnly1(_, offset)) }
    val captureOnly = oneStep(_: Square, _: (Int, Int), opponent.contains)
    val openOrCapture = oneStep(_: Square, _: (Int, Int), !same.contains(_))
    def openUntilCapture(start: Square, offset: (Int, Int)): Set[Square] = {
      var result = Set.empty[Square]; var c = start + offset
      while (c != Outside && !same.contains(c) && !opponent.contains(c)) { result = result + c; c = c + offset }
      if (c != Outside && opponent.contains(c)) result + c else result
    }

    def toMoves(start: Square, end: Square): Vector[Move] = Vector(Move.move(start, end))
    def toPromotions(start: Square, end: Square): Vector[Promotion] = PromotionType.all.map(t => Promotion(start, end, t))

    def captureIfPresent(s: Square): Side = if (opponent.contains(s)) opponent.remove(s, opponent.`type`(s)) else opponent
    def doMove(m: Move): Board = {
      val t = same.`type`(m.start)
      Board(captureIfPresent(m.end), same.add(m.end, t).remove(m.start, t))
    }
    def doPromotion(p: Promotion): Board =
      Board(captureIfPresent(p.end), same.remove(p.start, Pawn).add(p.end, p.`type`))

    def placesKingInCheck(b : Board) = Board.isCheck(b.opponent, b.same)
    def createLeaves[M <: MoveBase](starts: Set[Square],
                                    offsets: Vector[(Int, Int)],
                                    createEnds: (Square, (Int, Int)) => Set[Square],
                                    createMoves: (Square, Square) => Vector[M],
                                    createBoard: M => Board): Vector[(M, Board)] =
      starts.toVector.flatMap(s => offsets.flatMap(createEnds(s, _)).flatMap(e => createMoves(s, e)).map(m => (m, createBoard(m)))).filterNot(l => placesKingInCheck(l._2))

    val pawnLeaves = {
      val pawns = same.squares(Pawn)
      val promotionRank = if (same.color == White) rank(_7) else rank(_2)
      val advance = if (same.color == White) upOnly else downOnly
      val captures = if (same.color == White) upLeftUpRight else downLeftDownRight
      createLeaves(pawns & promotionRank, advance, openOnly1, toPromotions, doPromotion) ++
        createLeaves(pawns & promotionRank, captures, captureOnly, toPromotions, doPromotion) ++
        createLeaves(pawns.filterNot(same.moved), advance, openOnly2, toMoves, doMove) ++
        createLeaves(pawns.filter(same.moved) &~ promotionRank, advance, openOnly1, toMoves, doMove) ++
        createLeaves(pawns &~ promotionRank, captures, captureOnly, toMoves, doMove)
    }

    val castleLeaves: Vector[(Castle, Board)] = {

      val castleMoves = {
        def canCastle(rookStart: Square, between: Set[Square]): Boolean =
          !same.moved(E1) && !same.moved(rookStart) && between.forall(b => !same.contains(b) && !opponent.contains(b))
        val kingside = if (same.color == White) canCastle(H1, Set(F1, G1)) else canCastle(H8, Set(F8, G8))
        val queenside = if (same.color == White) canCastle(A1, Set(B1, C1, D1)) else canCastle(A8, Set(B8, C8, D8))
        if (kingside && queenside) Vector(`O-O`, `O-O-O`) else if (kingside) Vector(`O-O`) else if (queenside) Vector(`O-O-O`) else Vector.empty[Castle]
      }

      def castleBoard(kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square) =
        Board(opponent, same.remove(rookStart, Rook).remove(kingStart, King).add(rookEnd, Rook).add(kingEnd, King))

      def castlesThroughCheck(between: Set[Square]): Boolean =
        Board.isCheck(between.foldLeft(same)(_.add(_, King)), opponent)

      castleMoves
        .filter {
          case `O-O` => !castlesThroughCheck(if (same.color == White) Set(F1) else Set(F8))
          case `O-O-O` => !castlesThroughCheck(if (same.color == White) Set(C1, D1) else Set(C8, D8)) }
        .map {
          case `O-O` if same.color == White => `O-O` -> castleBoard(E1, G1, H1, F1)
          case `O-O` if same.color == Black => `O-O` -> castleBoard(E8, G8, H8, F8)
          case `O-O-O` if same.color == White => `O-O-O` -> castleBoard(E1, C1, A1, D1)
          case `O-O-O` if same.color == Black => `O-O-O` -> castleBoard(E8, C8, A8, D8) }

    }

    createLeaves(same.squares(Knight), knight, openOrCapture, toMoves, doMove) ++
      createLeaves(same.squares(Bishop), diagonals, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.squares(Rook), verticalHorizontal, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.squares(Queen), adjacent, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.squares(King), adjacent, openOrCapture, toMoves, doMove) ++ pawnLeaves ++ castleLeaves

  }

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

  def isCheck: Boolean = Board.isCheck(same, opponent)
  def isCheckmate: Boolean = moves.isEmpty && isCheck

  def isStalemate: Boolean = moves.isEmpty && !isCheck
  def isInsufficientMaterial: Boolean = {
    def both(t: PieceType): Set[Square] = same.squares(t) ++ opponent.squares(t)
    (both(Pawn).isEmpty || both(Rook).isEmpty || both(Queen).isEmpty) &&
      ((both(Knight).isEmpty || both(Bishop).isEmpty) || (both(Knight).size == 1 && both(Bishop).isEmpty) ||
       (both(Knight).isEmpty && (both(Bishop).forall(blackSquares.contains) || !both(Bishop).exists(blackSquares.contains))))
  }
  def isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  def isThreefoldRepetition: Boolean = false
  def isFiftyMoveRule: Boolean = false
  def mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match { case h :: t => leaves.find(_._1 == h).flatMap(_._2.move(t)); case _ => Some(this) }
  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

}
