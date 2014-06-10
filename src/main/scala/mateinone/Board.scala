package mateinone

import Square._
import Rank._
import scala.collection.mutable

object Board {

  private type Position = (Vector[Piece], Option[Int])

  private object Offsets {
    val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
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
    val open = Set(A3, A4, A5, A6, B3, B4, B5, B6, C3, C4, C5, C6, D3, D4, D5, D6, E3, E4, E5, E6, F3, F4, F5, F6, G3, G4, G5, G6, H3, H4, H5, H6)
    Board(White, same, opponent, open, Set.empty[Square], isCheck = false, 0, Vector.empty[Position], None)
  }

}
import Board._

case class Board private(
  turn: Side,
  private val same: Vector[Piece],
  private val opponent: Vector[Piece],
  private val open: Set[Square],
  private val moved: Set[Square],
  private val isCheck: Boolean,
  private val lastPawnMoveOrCapture: Int,
  private val positions: Vector[Board.Position],
  private val twoSquarePawnAdvance: Option[Int]
) {

  def pieces: Vector[Piece] = same ++ opponent

  private def position: Position = (pieces, twoSquarePawnAdvance)

  lazy val leaves: Vector[(MoveBase, Board)] = {

    def canCaptureKing(offense: Vector[Piece], defense: Vector[Piece], open: Set[Square]): Boolean = {
      def canCapture(start: Square, offset: (Int, Int), end: Square): Boolean = {
        var current = start + offset
        while (open.contains(current)) current = current + offset
        end == current
      }
      val king = defense.filter(_.`type` == King).map(_.square)
      def canCaptureKing(offset: (Int, Int), end: Square): Boolean = king.exists(canCapture(_, offset, end))
      def canCaptureKingInOne(offset: (Int, Int), end: Square): Boolean = king.exists(_ + offset == end)
      offense.exists {
        case Piece(White, Pawn, start) => Offsets.blackPawnCaptures.exists(canCaptureKingInOne(_, start))
        case Piece(Black, Pawn, start) => Offsets.whitePawnCaptures.exists(canCaptureKingInOne(_, start))
        case Piece(_, Knight, start) => Offsets.knight.exists(canCaptureKingInOne(_, start))
        case Piece(_, Bishop, start) => Offsets.diagonals.exists(canCaptureKing(_, start))
        case Piece(_, Rook, start) => Offsets.rook.exists(canCaptureKing(_, start))
        case Piece(_, Queen, start) => Offsets.queen.exists(canCaptureKing(_, start))
        case Piece(_, King, start) => Offsets.adjacent.exists(canCaptureKingInOne(_, start))
      }
    }

    def legalAndIllegal: Vector[(MoveBase, Board)] = {

      def isOpponentSquare(s: Square): Boolean = opponent.exists(_.square == s)

      def toMoves(start: Square, ends: Vector[Square]): Vector[MoveBase] = ends.map(e => Move.move(start, e))

      def ends(start: Square, offset: (Int, Int)): Vector[Square] = {
        val result = new mutable.Stack[Square]
        var current = start + offset
        while (open.contains(current)) { result.push(current); current = current + offset }
        (if (isOpponentSquare(current)) result.push(current) else result).toVector
      }

      def pawnMoves(start: Square, advance: Vector[(Int, Int)], captures: Vector[(Int, Int)], initial: Int, promotion: Int, enPassant: Int): Vector[MoveBase] = {
        def enPassantEnd(captureOffset: (Int, Int), moveOffset: (Int, Int)): Option[Square] =
          for {
            last <- twoSquarePawnAdvance
            captureSquare = start + captureOffset
            if isOpponentSquare(captureSquare)
            if captureSquare.file == last
            stepSquare = start + moveOffset
          } yield stepSquare
        def toPromotions(start: Square, ends: Vector[Square]): Vector[MoveBase] =
          ends.flatMap(e => PromotionType.all.map(t => Promotion(start, e, t)))
        val captureEnds: Vector[Square] = captures.map(start + _).filter(isOpponentSquare)
        def advanceOne(s: Square): Vector[Square] = advance.map(s + _).filter(open.contains)
        val advanceEnds: Vector[Square] =
          if (start.rank == initial) { val ones = advanceOne(start); ones ++ ones.flatMap(advanceOne) } else advanceOne(start)
        if (start.rank == promotion) toPromotions(start, captureEnds ++ advanceEnds)
        else if (start.rank == enPassant) toMoves(start, captureEnds ++ advanceEnds ++ Offsets.enPassantCaptures.zip(captures).flatMap { case (c, m) => enPassantEnd(c, m) })
        else toMoves(start, captureEnds ++ advanceEnds)
      }

      def doMove(move: MoveBase): Board = {
        def board(start: Square, end: Square, `type`: PieceType, last: Int, positions: Vector[Position], two: Option[Int]) = {
          val sameNext = opponent.filterNot(_.square == end)
          val opponentNext = same.filterNot(_.square == start) :+ Piece.piece(turn, `type`, end)
          val openNext = open + start - end
          val movedNext = moved + end
          Board(turn.other, sameNext, opponentNext, openNext, movedNext, canCaptureKing(opponentNext, sameNext, openNext), last, positions, two)
        }
        def castleBoard(kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square): Board = {
          val sameNext = opponent
          val opponentNext = same.filterNot(_.square == kingStart).filterNot(_.square == rookStart) ++ Vector(Piece.piece(turn, King, kingEnd), Piece.piece(turn, Rook, rookEnd))
          val openNext = open + kingStart + rookStart - kingEnd - rookEnd
          val movedNext = moved + kingEnd + rookEnd
          Board(turn.other, sameNext, opponentNext, openNext, movedNext, canCaptureKing(opponentNext, sameNext, openNext), lastPawnMoveOrCapture + 1, Vector.empty[Position], None)
        }
        move match {
          case Move(start, end) =>
            val `type` = same.find(_.square == start).get.`type`
            val capture = opponent.map(_.square).contains(end)
            val last = if (capture || `type` == Pawn) 0 else lastPawnMoveOrCapture + 1
            val two = if (`type` == Pawn && math.abs(end.rank - start.rank) == 2) Some(end.file) else None
            board(start, end, `type`, last, positions :+ position, two)
          case Promotion(start, end, promotion) => board(start, end, promotion, 0, Vector.empty[Position], None)
          case `O-O` if turn == White => castleBoard(E1, G1, H1, F1)
          case `O-O` if turn == Black => castleBoard(E8, G8, H8, F8)
          case `O-O-O` if turn == White => castleBoard(E1, C1, A1, D1)
          case `O-O-O` if turn == Black => castleBoard(E8, C8, A8, D8)
        }
      }

      val moves: Vector[MoveBase] = same.flatMap {
        case Piece(White, Pawn, start) => pawnMoves(start, Offsets.whitePawnAdvance, Offsets.whitePawnCaptures, `_2`, `_7`, `_5`)
        case Piece(Black, Pawn, start) => pawnMoves(start, Offsets.blackPawnAdvance, Offsets.blackPawnCaptures, `_7`, `_2`, `_4`)
        case Piece(_, Knight, start) => toMoves(start, Offsets.knight.map(start + _).filter(s => open.contains(s) || isOpponentSquare(s)))
        case Piece(_, Bishop, start) => toMoves(start, Offsets.diagonals.flatMap(ends(start, _)))
        case Piece(_, Rook, start) => toMoves(start, Offsets.rook.flatMap(ends(start, _)))
        case Piece(_, Queen, start) => toMoves(start, Offsets.queen.flatMap(ends(start, _)))
        case Piece(_, King, start) => toMoves(start, Offsets.adjacent.map(start + _).filter(s => open.contains(s) || isOpponentSquare(s)))
      }

      val castles: Vector[MoveBase] = {
        def canCastle(rookStart: Square, between: Set[Square]): Boolean = !moved.contains(E1) && !moved.contains(rookStart) && between.forall(open.contains)
        val kingside = if (turn == White) canCastle(H1, Set(F1, G1)) else canCastle(H8, Set(F8, G8))
        val queenside = if (turn == White) canCastle(A1, Set(B1, C1, D1)) else canCastle(A8, Set(B8, C8, D8))
        if (kingside && queenside) Vector(`O-O`, `O-O-O`) else if (kingside) Vector(`O-O`) else if (queenside) Vector(`O-O-O`) else Vector()
      }

      (moves ++ castles).map(m => (m, doMove(m)))
    }

    def castlesThroughCheck(between: Vector[Square]): Boolean = canCaptureKing(opponent, same ++ between.map(Piece.piece(turn, King, _)), open -- between)

    legalAndIllegal
      .filterNot { case (_, b) => canCaptureKing(b.same, b.opponent, b.open) }
      .filter {
      case (`O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(F1) else Vector(F8))
      case (`O-O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(C1, D1) else Vector(C8, D8))
      case _ => true }

  }

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

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
