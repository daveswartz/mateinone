package mateinone

import Square._
import Rank._
import scala.collection.mutable

object Board {

  private type Position = (Vector[Piece], Option[Int])

  private object Offsets {
    val (up, upRight, right, downRight, down, downLeft, left, upLeft) =
      ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
    val (file, rank) = (Vector(right, left), Vector(up, down))
    val diagonals = Vector(upRight, downRight, downLeft, upLeft)
    val adjacent = Vector(up, upRight, right, downRight, down, downLeft, left, upLeft)
    val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
    val rook = file ++ rank
    val queen = rook ++ diagonals
    val whitePawnCaptures = Vector(upLeft, upRight)
    val whitePawnAdvance = Vector(up)
    val blackPawnCaptures = Vector(downLeft, downRight)
    val blackPawnAdvance = Vector(down)
    val enPassantCaptures = Vector(left, right)
  }

  def initial: Board = {
    import Piece.piece
    val same = Vector(
      piece(White, Rook, A1), piece(White, Pawn, A2), piece(White, Knight, B1), piece(White, Pawn, B2),
      piece(White, Bishop, C1), piece(White, Pawn, C2), piece(White, Queen, D1), piece(White, Pawn, D2),
      piece(White, King, E1), piece(White, Pawn, E2), piece(White, Bishop, F1), piece(White, Pawn, F2),
      piece(White, Knight, G1), piece(White, Pawn, G2), piece(White, Rook, H1), piece(White, Pawn, H2))
    val opponent = Vector(
      piece(Black, Rook, A8), piece(Black, Pawn, A7), piece(Black, Knight, B8), piece(Black, Pawn, B7),
      piece(Black, Bishop, C8), piece(Black, Pawn, C7), piece(Black, Queen, D8), piece(Black, Pawn, D7),
      piece(Black, King, E8), piece(Black, Pawn, E7), piece(Black, Bishop, F8), piece(Black, Pawn, F7),
      piece(Black, Knight, G8), piece(Black, Pawn, G7), piece(Black, Rook, H8), piece(Black, Pawn, H7))
    val open = Set(A3, A4, A5, A6, B3, B4, B5, B6, C3, C4, C5, C6, D3, D4, D5, D6,
      E3, E4, E5, E6, F3, F4, F5, F6, G3, G4, G5, G6, H3, H4, H5, H6)
    Board(White, same, opponent, open, Set.empty[Square], 0, Vector.empty[Position], None)
  }

  private object CheckConstants {
    val up_left_down_right = Vector(Offsets.up, Offsets.right, Offsets.down, Offsets.left)
    val upleft_upright = Vector(Offsets.upLeft, Offsets.upRight)
    val downleft_downright = Vector(Offsets.downLeft, Offsets.downRight)
    val knight = Offsets.knight
  }

  private def isCheck(attackers: Vector[Piece], kings: Vector[Piece], same: Set[Square], open: Set[Square]): Boolean = {
    import CheckConstants._

    def givingCheck(king: Square, offset: (Int, Int), inOneStep: Piece => Boolean, inAnySteps: Piece => Boolean): Boolean = {
      var current = king + offset
      if (current == outside || same.contains(current)) false
      else {
        var steps = 1
        while (open.contains(current)) { current = current + offset; steps += 1 }
        if (current == outside || same.contains(current)) false
        else if (steps == 1) attackers.filter(_.square == current).exists(inOneStep)
        else attackers.filter(_.square == current).exists(inAnySteps)
      }
    }

    def isType(p: Piece, t: PieceType*) = t.exists(p.`type` ==)
    def isSideAndType(p: Piece, s: Side, t: PieceType) = p.side == s &&  p.`type` == t

    kings.exists { case Piece(_, King, king) =>
      up_left_down_right.exists(givingCheck(king, _, isType(_, Rook, Queen, King), isType(_, Rook, Queen))) ||
        upleft_upright.exists(givingCheck(king, _, p => isSideAndType(p, Black, Pawn) || isType(p, Bishop, Queen, King), isType(_, Bishop, Queen))) ||
        downleft_downright.exists(givingCheck(king, _, p => isSideAndType(p, White, Pawn) || isType(p, Bishop, Queen, King), isType(_, Bishop, Queen))) ||
        knight.exists { offset => val current = king + offset; attackers.exists(a => a.`type` == Knight && a.square == current) }
    }
  }

}
import Board._

case class Board private(
  turn: Side,
  private val same: Vector[Piece],
  private val opponent: Vector[Piece],
  private val open: Set[Square],
  private val moved: Set[Square],
  private val lastPawnMoveOrCapture: Int,
  private val positions: Vector[Board.Position],
  private val twoSquarePawnAdvance: Option[Int]
) {

  lazy val pieces: Vector[Piece] = same ++ opponent

  private def position: Position = (pieces, twoSquarePawnAdvance)

  lazy val leaves: Vector[(MoveBase, Board)] = {

    def createBoard(start: Square, end: Square, `type`: PieceType, last: Int, positions: Vector[Position], two: Option[Int]) = {
      val pieceAfter = Piece.piece(turn, `type`, end)
      val sameAfter = same.filterNot(_.square == start) :+ pieceAfter
      val opponentAfter = opponent.filterNot(_.square == end)
      val openAfter = open + start - end
      Board(turn.other, opponentAfter, sameAfter, openAfter, moved + end, last, positions, two)
    }

    def doMove(m: Move): Board = {
      val `type` = same.find(_.square == m.start).get.`type`
      val capture = opponent.map(_.square).contains(m.end)
      val last = if (capture || `type` == Pawn) 0 else lastPawnMoveOrCapture + 1
      val two = if (`type` == Pawn && math.abs(m.end.rank - m.start.rank) == 2) Some(m.end.file) else None
      createBoard(m.start, m.end, `type`, last, positions :+ position, two)
    }

    def doPromotion(p: Promotion): Board = createBoard(p.start, p.end, p.`type`, 0, Vector.empty[Position], None)

    def createLeaves[M <: MoveBase](somePieces: Vector[Piece],
                     offsets: Vector[(Int, Int)],
                     createEnds: (Square, (Int, Int)) => Vector[Square],
                     createMoves: (Square, Square) => Vector[M],
                     createBoard: M => Board): Vector[(M, Board)] =
      somePieces // TODO only pieces that could capture the moved piece can cause check
        .flatMap(p => offsets.flatMap(createEnds(p.square, _)).flatMap(end => createMoves(p.square, end)).map(m => (m, createBoard(m))))
        .filterNot { case (_, b) => Board.isCheck(b.same, b.opponent.filter(_.`type` == King), b.opponent.map(_.square).toSet, b.open) }

    def openOrCapture(start: Square, offset: (Int, Int)): Vector[Square] = {
      val end = start + offset
      if (open.contains(end) || opponent.exists(_.square == end)) Vector(end) else Vector.empty[Square]
    }

    def openUntilCapture(start: Square, offset: (Int, Int)): Vector[Square] = {
      val result = new mutable.Stack[Square]
      var current = start + offset
      while (open.contains(current)) {
        result.push(current); current = current + offset
      }
      (if (opponent.exists(_.square == current)) result.push(current) else result).toVector
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
      if (opponent.exists(_.square == end)) Vector(end) else Vector.empty[Square]
    }

    def toMoves(start: Square, end: Square): Vector[Move] = Vector(Move.move(start, end))

    def toPromotions(start: Square, end: Square): Vector[Promotion] = PromotionType.all.map(t => Promotion(start, end, t))

    val pawnLeaves = {
      val pawns = same.filter(_.`type` == Pawn)
      val initialRank = if (turn == White) `_2` else `_7`
      val promotionRank = if (turn == White) `_7` else `_2`
      val enPassantRank = if (turn == White) `_5` else `_4`
      val advance = if (turn == White) Offsets.whitePawnAdvance else Offsets.blackPawnAdvance
      val captures = if (turn == White) Offsets.whitePawnCaptures else Offsets.blackPawnCaptures
      createLeaves(pawns.filter(p => p.square.rank == initialRank), advance, openToOccupied2, toMoves, doMove) ++
        createLeaves(pawns.filter(p => p.square.rank > `_2` && p.square.rank < `_7`), advance, openToOccupied1, toMoves, doMove) ++
        createLeaves(pawns.filter(p => p.square.rank == promotionRank), advance, openToOccupied1, toPromotions, doPromotion) ++
        createLeaves(pawns.filter(p => p.square.rank != promotionRank), captures, captureOnly, toMoves, doMove) ++
        createLeaves(pawns.filter(p => p.square.rank == promotionRank), captures, captureOnly, toPromotions, doPromotion) ++
        twoSquarePawnAdvance.toVector.flatMap { file =>
          def adjacent(a: Square, b: Square) = a == Square.square(b.file+1, b.rank) || a == Square.square(b.file-1, b.rank)
            createLeaves(pawns.filter(p => adjacent(Square.square(file, enPassantRank), p.square)), captures, (s, o) => Vector(s + o), toMoves, doMove) }
    }

    val castleLeaves: Vector[(Castle, Board)] = {

      def canCastle(rookStart: Square, between: Set[Square]): Boolean =
        !moved.contains(E1) && !moved.contains(rookStart) && between.forall(open.contains)

      val kingside = if (turn == White) canCastle(H1, Set(F1, G1)) else canCastle(H8, Set(F8, G8))
      val queenside = if (turn == White) canCastle(A1, Set(B1, C1, D1)) else canCastle(A8, Set(B8, C8, D8))

      val castleMoves = if (kingside && queenside) Vector(`O-O`, `O-O-O`) else if (kingside) Vector(`O-O`) else if (queenside) Vector(`O-O-O`) else Vector()

      def castleBoard(kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square): Board = {
        val sameNext = opponent
        val opponentNext = same.filterNot(_.square == kingStart).filterNot(_.square == rookStart) ++ Vector(Piece.piece(turn, King, kingEnd), Piece.piece(turn, Rook, rookEnd))
        val openNext = open + kingStart + rookStart - kingEnd - rookEnd
        val movedNext = moved + kingEnd + rookEnd
        Board(turn.other, sameNext, opponentNext, openNext, movedNext, lastPawnMoveOrCapture + 1, Vector.empty[Position], None) }

      def castlesThroughCheck(between: Vector[Square]): Boolean =
        Board.isCheck(opponent, same.filter(_.`type` == King) ++ between.map(Piece.piece(turn, King, _)), same.map(_.square).toSet, open -- between)

      castleMoves
        .filter {
          case `O-O` => !castlesThroughCheck(if (turn == White) Vector(F1) else Vector(F8))
          case `O-O-O` => !castlesThroughCheck(if (turn == White) Vector(C1, D1) else Vector(C8, D8)) }
        .map {
          case `O-O` if turn == White => `O-O` -> castleBoard(E1, G1, H1, F1)
          case `O-O` if turn == Black => `O-O` -> castleBoard(E8, G8, H8, F8)
          case `O-O-O` if turn == White => `O-O-O` -> castleBoard(E1, C1, A1, D1)
          case `O-O-O` if turn == Black => `O-O-O` -> castleBoard(E8, C8, A8, D8) }

    }

    createLeaves(same.filter(_.`type` == Knight), Offsets.knight, openOrCapture, toMoves, doMove) ++
      createLeaves(same.filter(_.`type` == Bishop), Offsets.diagonals, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.filter(_.`type` == Rook), Offsets.rook, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.filter(_.`type` == Queen), Offsets.queen, openUntilCapture, toMoves, doMove) ++
      createLeaves(same.filter(_.`type` == King), Offsets.adjacent, openOrCapture, toMoves, doMove) ++
      pawnLeaves ++
      castleLeaves

  }

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

  def isCheck: Boolean = Board.isCheck(opponent, same.filter(_.`type` == King), same.map(_.square).toSet, open)
  def isCheckmate: Boolean = moves.isEmpty && isCheck

  def isStalemate: Boolean = moves.isEmpty && !isCheck
  def isInsufficientMaterial: Boolean = {
    val byType = pieces.groupBy(_.`type`)
    def isBlack(s: Square) = s.file % 2 == 0 && s.file % 2 == 0
    byType.keySet == Set(King) ||
      (byType.keySet == Set(King, Knight) && byType(Knight).size == 1) ||
      (byType.keySet == Set(King, Bishop) && byType(Bishop).map(b => isBlack(b.square)).size == 1)
  }
  def isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  def isThreefoldRepetition: Boolean = {
    def castlingRights(p: Position) = p._1.filter(p => p.`type` == King || p.`type` == Rook).toSet
    def otherPieces(p: Position) = p._1.filter(p => p.`type` != King && p.`type` != Rook).map(p => (p.side, p.`type`, p.square)).toSet
    positions.count(other =>
      castlingRights(other) == castlingRights(position) && otherPieces(other) == otherPieces(position) && other._2 == position._2
    ) == 2
  }
  def isFiftyMoveRule: Boolean = lastPawnMoveOrCapture >= 100
  def mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match { case h :: t => leaves.find(_._1 == h).flatMap(_._2.move(t)); case _ => Some(this) }

  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString: String = "Board("+turn+","+pieces+")"

}
