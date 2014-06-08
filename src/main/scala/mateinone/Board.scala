package mateinone

import Square._
import File._
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

  def initial: Board = { // TODO just hard code initial board state and moves
    def sidePieces(side: Side, second: Int => Square, back: Int => Square): Vector[Piece] = {
      def piece(`type`: PieceType, rank: Int => Square, files: Vector[Int]) = files.map(f => Piece.piece(side, `type`, rank(f), hasMoved = false))
      piece(Pawn, second, files) ++ piece(Rook, back, Vector(A, H)) ++ piece(Knight, back, Vector(B, G)) ++ piece(Bishop, back, Vector(C, F)) ++
        piece(King, back, Vector(E)) ++ piece(Queen, back, Vector(D))
    }
    val whitePieces = sidePieces(White, square(_, _2), square(_, _1))
    val blackPieces = sidePieces(Black, square(_, _7), square(_, _8))
    val open: Set[Square] = Square.squares.flatten.toSet diff (whitePieces ++ blackPieces).map(_.square).toSet
    Board(White, whitePieces, blackPieces, open, 0, Vector.empty[Position], None)
  }

}
import Board._

case class Board private(
  turn: Side,
  private val same: Vector[Piece],
  private val opponent: Vector[Piece],
  private val open: Set[Square],
  private val lastPawnMoveOrCapture: Int,
  private val positions: Vector[Board.Position],
  private val twoSquarePawnAdvance: Option[Int]
) {

  def pieces: Vector[Piece] = same ++ opponent

  private def isOpponentSquare(s: Square): Boolean = opponent.exists(_.square == s)

  private def legalAndIllegal: Vector[(MoveBase, Board)] = { // TODO much duplication here, figure out how to hard code initial moves and simplify to update

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
      def castleBoard(kingStart: Square, kingEnd: Square, rookStart: Square, rookEnd: Square): Board =
        Board(turn.other, opponent, same.filterNot(_.square == kingStart).filterNot(_.square == rookStart) ++ Vector(Piece.piece(turn, King, kingEnd, hasMoved = true), Piece.piece(turn, Rook, rookEnd, hasMoved = true)), open + kingStart + rookStart - kingEnd - rookEnd, lastPawnMoveOrCapture + 1, Vector.empty[Position], None)
      move match {
        case Move(start, end) =>
          val `type` = same.find(_.square == start).get.`type`
          val capture = opponent.map(_.square).contains(end)
          Board(
            turn.other,
            if (capture) opponent.filterNot(_.square == end) else opponent,
            same.filterNot(_.square == start) :+ Piece.piece(turn, `type`, end, hasMoved = true),
            open + start - end,
            if (capture || `type` == Pawn) 0 else lastPawnMoveOrCapture + 1,
            positions :+ position,
            if (`type` == Pawn && math.abs(end.rank - start.rank) == 2) Some(end.file) else None)
        case Promotion(start, end, promotion) =>
          Board(
            turn.other,
            opponent.filterNot(_.square == end),
            same.filterNot(_.square == start) :+ Piece.piece(turn, promotion, end, hasMoved = true),
            open + start - end,
            0,
            Vector.empty[Position],
            None)
        case `O-O` if turn == White => castleBoard(E1, G1, H1, F1)
        case `O-O` if turn == Black => castleBoard(E8, G8, H8, F8)
        case `O-O-O` if turn == White => castleBoard(E1, C1, A1, D1)
        case `O-O-O` if turn == Black => castleBoard(E8, C8, A8, D8)
      }
    }

    val moves: Vector[MoveBase] = same.flatMap {
      case Piece(White, Pawn, start, _) => pawnMoves(start, Offsets.whitePawnAdvance, Offsets.whitePawnCaptures, `_2`, `_7`, `_5`)
      case Piece(Black, Pawn, start, _) => pawnMoves(start, Offsets.blackPawnAdvance, Offsets.blackPawnCaptures, `_7`, `_2`, `_4`)
      case Piece(_, Knight, start, _) => toMoves(start, Offsets.knight.map(start + _).filter(s => open.contains(s) || isOpponentSquare(s)))
      case Piece(_, Bishop, start, _) => toMoves(start, Offsets.diagonals.flatMap(ends(start, _)))
      case Piece(_, Rook, start, _) => toMoves(start, Offsets.rook.flatMap(ends(start, _)))
      case Piece(_, Queen, start, _) => toMoves(start, Offsets.queen.flatMap(ends(start, _)))
      case Piece(_, King, start, _) => toMoves(start, Offsets.adjacent.map(start + _).filter(s => open.contains(s) || isOpponentSquare(s)))
    }

    val castles: Vector[MoveBase] = {
      val isWhite: Boolean = turn == White
      val stationary: Vector[Square] = same.filter(!_.hasMoved).map(_.square)
      def hasMoved(s: Square): Boolean = !stationary.contains(s)
      val kingMoved = hasMoved(if (isWhite) E1 else E8)
      val kingside: Boolean = !hasMoved(if (isWhite) H1 else H8) && !kingMoved && (if (isWhite) Vector(F1, G1) else Vector(F8, G8)).forall(open.contains)
      val queenside: Boolean = !hasMoved(if (isWhite) A1 else A8) && !kingMoved && (if (isWhite) Vector(B1, C1, D1) else Vector(B8, C8, D8)).forall(open.contains)
      if (kingside && queenside) Vector(`O-O`, `O-O-O`) else if (kingside) Vector(`O-O`) else if (queenside) Vector(`O-O-O`) else Vector()
    }

    (moves ++ castles).map(m => (m, doMove(m)))
  }

  private lazy val canCaptureKing: Boolean = {
    def canCapture(start: Square, offset: (Int, Int), end: Square): Boolean = {
      var current = start + offset
      while (open.contains(current)) current = current + offset
      end == current
    }
    val opponentsKing = opponent.filter(_.`type` == King).map(_.square)
    def canCaptureKing(offset: (Int, Int), end: Square): Boolean = opponentsKing.exists(canCapture(_, offset, end))
    def canCaptureKingInOne(offset: (Int, Int), end: Square): Boolean = opponentsKing.exists(_ + offset == end)
    same.exists {
      case Piece(White, Pawn, start, _) => Offsets.blackPawnCaptures.exists(canCaptureKingInOne(_, start))
      case Piece(Black, Pawn, start, _) => Offsets.whitePawnCaptures.exists(canCaptureKingInOne(_, start))
      case Piece(_, Knight, start, _) => Offsets.knight.exists(canCaptureKingInOne(_, start))
      case Piece(_, Bishop, start, _) => Offsets.diagonals.exists(canCaptureKing(_, start))
      case Piece(_, Rook, start, _) => Offsets.rook.exists(canCaptureKing(_, start))
      case Piece(_, Queen, start, _) => Offsets.queen.exists(canCaptureKing(_, start))
      case Piece(_, King, start, _) => Offsets.adjacent.exists(canCaptureKingInOne(_, start))
    }
  }

  lazy val leaves: Vector[(MoveBase, Board)] = {
    def castlesThroughCheck(between: Vector[Square]): Boolean =
      this.copy(same = same ++ between.map(Piece.piece(turn, King, _, hasMoved = true))).isCheck
    legalAndIllegal
      .filter(!_._2.canCaptureKing)
      .filter {
        case (`O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(F1) else Vector(F8))
        case (`O-O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(C1, D1) else Vector(C8, D8))
        case _ => true }
  }

  private lazy val position: Position = (pieces, twoSquarePawnAdvance)

  def moves: Vector[MoveBase] = leaves.map(_._1)
  def boards: Vector[Board] = leaves.map(_._2)

  def isCheck: Boolean = this.copy(same = opponent, opponent = same, turn = turn.other).canCaptureKing
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
