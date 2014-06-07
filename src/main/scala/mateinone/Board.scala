package mateinone

import Square._
import File._
import Rank._
import scala.collection.mutable

object Board {

  private type Position = (Vector[Piece], Option[File])

  def initial: Board = {
    def sidePieces(side: Side, second: File => Square, back: File => Square): Vector[Piece] = {
      def piece(`type`: PieceType, rank: File => Square, files: Vector[File]) = files.map(f => Piece.piece(side, `type`, rank(f), hasMoved = false))
      piece(Pawn, second, files) ++ piece(Rook, back, Vector(A, H)) ++ piece(Knight, back, Vector(B, G)) ++ piece(Bishop, back, Vector(C, F)) ++
        piece(King, back, Vector(E)) ++ piece(Queen, back, Vector(D))
    }
    val whitePieces = sidePieces(White, square(_, _2), square(_, _1))
    val blackPieces = sidePieces(Black, square(_, _7), square(_, _8))
    val open: Set[Square] = Square.squares.flatten.toSet diff (whitePieces ++ blackPieces).map(_.square).toSet
    Board(White, whitePieces, blackPieces, open, 0, Vector.empty[Position], None)
  }

  private object Offsets {
    val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0:Byte, 1:Byte), (1:Byte, 1:Byte), (1:Byte, 0:Byte), (1:Byte, -1:Byte), (0:Byte, -1:Byte), (-1:Byte, -1:Byte), (-1:Byte, 0:Byte), (-1:Byte, 1:Byte))
    val (file, rank) = (Vector(right, left), Vector(up, down))
    val diagonals = Vector(upRight, downRight, downLeft, upLeft)
    val adjacent = Vector(up, upRight, right, downRight, down, downLeft, left, upLeft)
    val knight = Vector((2:Byte, 1:Byte), (2:Byte, -1:Byte), (1:Byte, 2:Byte), (1:Byte, -2:Byte), (-2:Byte, 1:Byte), (-2:Byte, -1:Byte), (-1:Byte, 2:Byte), (-1:Byte, -2:Byte))
    val rook = file ++ rank
    val queen = rook ++ diagonals
    val whitePawnCaptures = Vector(upLeft, upRight)
    val whitePawnAdvance = Vector(up)
    val blackPawnCaptures = Vector(downLeft, downRight)
    val blackPawnAdvance = Vector(down)
    val enPassantCaptures = Vector(left, right)
  }

  private def doMove(board: Board, move: MoveBase): Board = {
    val oneMove: PartialFunction[MoveBase, Board] = { case m @ Move(start, end) =>
      val `type` = board.same.find(_.square == start).get.`type`
      val capture = board.opponent.map(_.square).contains(end)
      val same = board.same.filterNot(_.square == start) :+ Piece.piece(board.turn, `type`, end, hasMoved = true)
      val opponent = if (capture) board.opponent.filterNot(_.square == end) else board.opponent
      val lastPawnMoveOrCapture = if (capture || `type` == Pawn) 0 else board.lastPawnMoveOrCapture + 1
      val twoSquarePawnAdvance = if (`type` == Pawn && math.abs(end.rank.n - start.rank.n) == 2) Some(end.file) else None
      Board(board.turn.other, opponent, same, board.open + start - end, lastPawnMoveOrCapture, board.positions :+ board.position, twoSquarePawnAdvance)
    }
    val onePromotion: PartialFunction[MoveBase, Board] = { case p @ Promotion(start, end, promotion) =>
      val same = board.same.filterNot(_.square == start) :+ Piece.piece(board.turn, promotion, end, hasMoved = true)
      val opponent = board.opponent.filterNot(_.square == end)
      Board(board.turn.other, opponent, same, board.open + start - end, 0, Vector.empty[Position], None)
    }
    val oneCastle: PartialFunction[MoveBase, Board] = { case c: Castle =>
      val back = if (board.turn == White) _1 else _8
      val (kingStart, kingEnd) = (square(E, back), c match { case `O-O` => square(G, back); case `O-O-O` => square(C, back) })
      val (rookStart, rookEnd) = c match { case `O-O` => (square(H, back), square(F, back)); case `O-O-O` => (square(A, back), square(D, back)) }
      val same = board.same.filterNot(_.square == kingStart).filterNot(_.square == rookStart) ++
        Vector(Piece.piece(board.turn, King, kingEnd, hasMoved = true), Piece.piece(board.turn, Rook, rookEnd, hasMoved = true))
      Board(board.turn.other, board.opponent, same, board.open + kingStart + rookStart - kingEnd - rookEnd, board.lastPawnMoveOrCapture + 1, Vector.empty[Position], None)
    }
    oneMove.orElse(onePromotion).orElse(oneCastle).apply(move)
  }

}
import Board._

case class Board private(
  turn: Side,
  same: Vector[Piece],
  opponent: Vector[Piece],
  private val open: Set[Square],
  private val lastPawnMoveOrCapture: Int,
  private val positions: Vector[Board.Position],
  private val twoSquarePawnAdvance: Option[File]
) {

  def pieces: Vector[Piece] = same ++ opponent

  private def isOpponentSquare(s: Square): Boolean = opponent.exists(_.square == s)

  private def legalAndIllegal: Vector[(MoveBase, Board)] = {
    import Offsets._

    def toMoves(start: Square, ends: Vector[Square]): Vector[MoveBase] = ends.map(e => Move.move(start, e))

    def ends(start: Square, offset: (Byte, Byte)): Vector[Square] = {
      val result = new mutable.Stack[Square]
      var current = start + offset
      while (current.exists(open.contains)) { result.push(current.get); current = current.flatMap(_ + offset) }
      (if (current.exists(isOpponentSquare)) result.push(current.get) else result).toVector
    }

    def pawnMoves(start: Square, advance: Vector[(Byte, Byte)], captures: Vector[(Byte, Byte)], initial: Rank, promotion: Rank, enPassant: Rank): Vector[MoveBase] = {
      def enPassantEnd(captureOffset: (Byte, Byte), moveOffset: (Byte, Byte)): Option[Square] =
        for {
          last <- twoSquarePawnAdvance
          captureSquare <- start + captureOffset
          if isOpponentSquare(captureSquare)
          if captureSquare.file == last
          stepSquare <- start + moveOffset
        } yield stepSquare
      def toPromotions(start: Square, ends: Vector[Square]): Vector[MoveBase] =
        ends.flatMap(e => PromotionType.all.map(t => Promotion(start, e, t)))
      val captureEnds = captures.flatMap(start + _).filter(isOpponentSquare)
      def advanceOne(s: Square) = advance.flatMap(s + _).filter(open.contains)
      val advanceEnds = if (start.rank == initial) { val ones = advanceOne(start); ones ++ ones.flatMap(advanceOne) } else advanceOne(start)
      if (start.rank == promotion) toPromotions(start, captureEnds ++ advanceEnds)
      else if (start.rank == enPassant) toMoves(start, captureEnds ++ advanceEnds ++ enPassantCaptures.zip(captures).flatMap { case (c, m) => enPassantEnd(c, m) })
      else toMoves(start, captureEnds ++ advanceEnds)
    }

    val moves: Vector[MoveBase] = same.flatMap {
      case Piece(White, Pawn, start, _) => pawnMoves(start, whitePawnAdvance, whitePawnCaptures, `_2`, `_7`, `_5`)
      case Piece(Black, Pawn, start, _) => pawnMoves(start, blackPawnAdvance, blackPawnCaptures, `_7`, `_2`, `_4`)
      case Piece(_, Knight, start, _) => toMoves(start, knight.flatMap(start + _).filter(s => open.contains(s) || isOpponentSquare(s)))
      case Piece(_, Bishop, start, _) => toMoves(start, diagonals.flatMap(ends(start, _)))
      case Piece(_, Rook, start, _) => toMoves(start, rook.flatMap(ends(start, _)))
      case Piece(_, Queen, start, _) => toMoves(start, queen.flatMap(ends(start, _)))
      case Piece(_, King, start, _) => toMoves(start, adjacent.flatMap(start + _).filter(s => open.contains(s) || isOpponentSquare(s)))
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

    (moves ++ castles).map(m => (m, doMove(this, m)))
  }

  private lazy val canCaptureKing: Boolean = {
    def canCapture(start: Square, offset: (Byte, Byte), end: Square): Boolean = {
      var current = start + offset
      while (current.exists(open.contains)) current = current.flatMap(_ + offset)
      current.exists(end ==)
    }
    val opponentsKing = opponent.filter(_.`type` == King).map(_.square)
    def canCaptureKing(offset: (Byte, Byte), end: Square): Boolean = opponentsKing.exists(canCapture(_, offset, end))
    def canCaptureKingInOne(offset: (Byte, Byte), end: Square): Boolean = opponentsKing.exists(_ + offset == Some(end))
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

  def leaves: Vector[(MoveBase, Board)] = {
    def castlesThroughCheck(between: Vector[Square]): Boolean =
      this.copy(same = same ++ between.map(Piece.piece(turn, King, _, hasMoved = true))).isCheck
    legalAndIllegal
      .filter(!_._2.canCaptureKing)
      .filter {
        case (`O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(F1) else Vector(F8))
        case (`O-O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(C1, D1) else Vector(C8, D8))
        case _ => true }
  }

  private def position: Position = (pieces, twoSquarePawnAdvance)

  def moves: Vector[MoveBase] = leaves.map(_._1)
  lazy val boards: Vector[Board] = leaves.map(_._2)

  def isCheck: Boolean = this.copy(same = opponent, opponent = same, turn = turn.other).canCaptureKing
  def isCheckmate: Boolean = moves.isEmpty && isCheck

  def isStalemate: Boolean = moves.isEmpty && !isCheck
  def isInsufficientMaterial: Boolean = {
    val byType = pieces.groupBy(_.`type`)
    def isBlack(s: Square) = s.file.n % 2 == 0 && s.file.n % 2 == 0
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

  override def toString(): String = "Board("+turn+","+pieces+")"

}
