package mateinone

import Square._
import File._
import Rank._

object Board {

  val initial: Board = {
    def pieces(side: Side, second: File => Square, back: File => Square) = {
      def piece(`type`: PieceType, rank: File => Square, files: Vector[File]) = files.map(f => Piece(side, `type`, rank(f), hasMoved = false))
      piece(Pawn, second, files) ++ piece(Rook, back, Vector(A, H)) ++ piece(Knight, back, Vector(B, G)) ++ piece(Bishop, back, Vector(C, F)) ++
        piece(King, back, Vector(E)) ++ piece(Queen, back, Vector(D))
    }
    Board(White, pieces(White, square(_, _2), square(_, _1)) ++ pieces(Black, square(_, _7), square(_, _8)), Vector())
  }

  private def between(c: Castle, s: Side): Vector[Square] = c match {
    case `O-O` => s match { case White => Vector(F1, G1); case Black => Vector(F8, G8) }
    case `O-O-O` => s match { case White => Vector(B1, C1, D1); case Black => Vector(B8, C8, D8) }
  }

  private object Offsets {
    val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
    val (file, rank) = (List(right, left), List(up, down))
    val diagonals = List(upRight, downRight, downLeft, upLeft)
    val adjacent = List(up, upRight, right, downRight, down, downLeft, left, upLeft)
    val knight = List((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
  }

  private def generateMoves(board: Board, piece: Piece): Iterable[MoveBase] = {

    def isOccupied(square: Square) = board.pieces exists(_.square == square)
    def isCapture(square: Square) = board.pieces exists(p => p.square == square && p.side == piece.side.other)
    def steps(step: (Int, Int)) = Iterator.iterate[Option[Square]](Some(piece.square))(_.flatMap(_ + step)).drop(1).takeWhile(_.isDefined).map(_.get)
    def pawnAdvance(step: (Int, Int), nSteps: Int) = steps(step).take(nSteps).takeWhile(!isOccupied(_))
    def pawnCapture(step: (Int, Int)) = steps(step).take(1).takeWhile(isCapture)
    def canCapture(step: (Int, Int)) = steps(step).span(!isOccupied(_)) match { case (empty, occ) => empty ++ occ.take(1).takeWhile(isCapture) }
    def file = Offsets.file.map(canCapture)
    def rank = Offsets.rank.map(canCapture)
    def diagonals = Offsets.diagonals.map(canCapture)
    def adjacent = Offsets.adjacent.map(canCapture(_).take(1))

    def enPassant(start: File, rankStep: Int): Option[Iterator[Square]] = {
      val (lastMove, lastBoard) = board.history.last
      def isAllowed(last: Move, fileStep: Int) = {
        val isPawn = lastBoard.pieces.find(_.square == last.start).get.`type` == Pawn
        val isTwoSquareAdvance = math.abs(last.end.rank.n - last.start.rank.n) == 2
        val canCapture = start + fileStep == Some(last.end.file)
        isPawn && isTwoSquareAdvance && canCapture
      }
      lastMove match {
        case last: Move if isAllowed(last, 1) => Some(canCapture((1, rankStep)))
        case last: Move if isAllowed(last, -1) => Some(canCapture((-1, rankStep)))
        case _=> None
      }
    }

    def castles(side: Side): Vector[Castle] = {
      def canCastle(c: Castle) = {
        def rook(c: Castle): Square = c match { case `O-O` => side match { case White => H1; case Black => H8 }; case `O-O-O` => side match { case White => A1; case Black => A8 } }
        board.pieces.exists(p => p.square == rook(c) && !p.hasMoved) && between(c, side).forall(s => !board.pieces.exists(_.square == s))
      }
      var result = Vector.empty[Castle]
      if (canCastle(`O-O`)) result = result :+ `O-O`
      if (canCastle(`O-O-O`)) result = result :+ `O-O-O`
      result
    }

    def toMoves(paths: Iterable[Iterator[Square]]) = paths.flatMap(_.map(e => Move(piece.square, e)))
    def toPromotions(paths: Iterable[Iterator[Square]]) = paths.flatMap(_.flatMap(e => PromotionType.all.map(t => Promotion(piece.square, e, t))))

    piece match {
      case Piece(White, Pawn,   Square(_, `_2`), false) => toMoves(Iterable(pawnCapture(Offsets.upRight), pawnCapture(Offsets.upLeft), pawnAdvance(Offsets.up, 2)))
      case Piece(White, Pawn,   Square(f, `_5`), true ) => toMoves(Iterable(pawnCapture(Offsets.upRight), pawnCapture(Offsets.upLeft), pawnAdvance(Offsets.up, 1)) ++ enPassant(f, 1))
      case Piece(White, Pawn,   Square(_, `_7`), true ) => toPromotions(Iterable(pawnCapture(Offsets.upRight), pawnCapture(Offsets.upLeft), pawnAdvance(Offsets.up, 1)))
      case Piece(White, Pawn,   _,               true ) => toMoves(Iterable(pawnCapture(Offsets.upRight), pawnCapture(Offsets.upLeft), pawnAdvance(Offsets.up, 1)))
      case Piece(White, King,   E1,              false) => toMoves(adjacent) ++ castles(White)
      case Piece(Black, Pawn,   Square(_, `_7`), false) => toMoves(Iterable(pawnCapture(Offsets.downRight), pawnCapture(Offsets.downLeft), pawnAdvance(Offsets.down, 2)))
      case Piece(Black, Pawn,   Square(f, `_4`), true ) => toMoves(Iterable(pawnCapture(Offsets.downRight), pawnCapture(Offsets.downLeft), pawnAdvance(Offsets.down, 1)) ++ enPassant(f, -1))
      case Piece(Black, Pawn,   Square(_, `_2`), true ) => toPromotions(Iterable(pawnCapture(Offsets.downRight), pawnCapture(Offsets.downLeft), pawnAdvance(Offsets.down, 1)))
      case Piece(Black, Pawn,   _,               true ) => toMoves(Iterable(pawnCapture(Offsets.downRight), pawnCapture(Offsets.downLeft), pawnAdvance(Offsets.down, 1)))
      case Piece(Black, King,   E8,              false) => toMoves(adjacent) ++ castles(Black)
      case Piece(_,     Rook,   _,               _    ) => toMoves(file ++ rank)
      case Piece(_,     Knight, _,               _    ) => toMoves(Offsets.knight.map(canCapture(_).take(1)))
      case Piece(_,     Bishop, _,               _    ) => toMoves(diagonals)
      case Piece(_,     King,   _,               _    ) => toMoves(adjacent)
      case Piece(_,     Queen,  _,               _    ) => toMoves(file ++ rank ++ diagonals)
    }

  }

  private def doMove(board: Board, move: MoveBase): Board = {
    def movePieces(ps: Vector[Piece], start: Square, end: Square, `type`: PieceType) =
      ps.filterNot(_.square == start).filterNot(_.square == end) :+ Piece(board.turn, `type`, end, hasMoved = true)
    def createBoard(pieces: Vector[Piece], last: MoveBase) = Board(board.turn.other, pieces, board.history :+ (move, board))
    val oneMove: PartialFunction[MoveBase, Board] = { case m @ Move(start, end) =>
      createBoard(movePieces(board.pieces, start, end, board.pieces.find(_.square == start).get.`type`), m) }
    val onePromotion: PartialFunction[MoveBase, Board] = { case p @ Promotion(start, end, promotion) =>
      createBoard(movePieces(board.pieces, start, end, promotion), p) }
    val oneCastle: PartialFunction[MoveBase, Board] = { case c: Castle =>
      def doCastle(back: Rank) = {
        def moveKing(ps: Vector[Piece], end: File) = movePieces(ps, square(E, back), square(end, back), King)
        def moveRook(ps: Vector[Piece], start: File, end: File) = movePieces(ps, square(start, back), square(end, back), Rook)
        c match { case `O-O` => moveRook(moveKing(board.pieces, G), H, F); case `O-O-O` => moveRook(moveKing(board.pieces, C), A, D) } }
      createBoard(doCastle(if (board.turn == White) _1 else _8), c) }
    oneMove.orElse(onePromotion).orElse(oneCastle).apply(move)
  }

}
import Board._

case class Board private(turn: Side, pieces: Vector[Piece], history: Vector[(MoveBase, Board)]) {

  private lazy val legalAndIllegal: Vector[MoveBase] = pieces filter(_.side == turn) flatMap(generateMoves(this, _))

  private lazy val opponentsKing = pieces filter(_.`type` == King) filter(_.side != turn)

  private lazy val canCaptureKing = legalAndIllegal exists { case s: StartAndEnd => opponentsKing.exists(_.square == s.end); case c: Castle => false }

  private lazy val leaves: Vector[(MoveBase, Board)] = {
    def castlesThroughCheck(m: MoveBase): Boolean = m match {
      case s: StartAndEnd => false
      case c: Castle =>
        val kings = between(c, turn) map (s => Piece(turn, King, s, hasMoved = true)) // TODO between is not between
        this.copy(pieces = pieces ++ kings).isCheck
    }
    legalAndIllegal map(m => (m, doMove(this, m))) filter { case (m, b) => !castlesThroughCheck(m) && !b.canCaptureKing }
  }

  lazy val moves: Vector[MoveBase] = leaves map { case (m, _) => m }
  lazy val boards: Vector[Board] = leaves map { case (_, b) => b }

  lazy val isCheck = this.copy(turn = turn.other).canCaptureKing
  lazy val isCheckmate = moves.isEmpty && isCheck

  lazy val isStalemate = moves.isEmpty && !isCheck
  lazy val isInsufficientMaterial = {
    val byType = pieces.groupBy(_.`type`)
    def isBlack(s: Square) = s.file.n % 2 == 0 && s.file.n % 2 == 0
    byType.keySet == Set(King) ||
      (byType.keySet == Set(King, Knight) && byType(Knight).size == 1) ||
      (byType.keySet == Set(King, Bishop) && byType(Bishop).map(b => isBlack(b.square)).distinct.size == 1)
  }
  lazy val isAutomaticDraw = isStalemate || isInsufficientMaterial

  lazy val isThreefoldRepetition = history.groupBy(_._2.moves).exists { case (_, repeats) => repeats.size == 3 }
  lazy val isFiftyMoveRule = {
    def isPawnMoveOrCapture(m: MoveBase, b: Board) = m match {
      case s: StartAndEnd => b.pieces.map(_.square).contains(s.end) || b.pieces.find(_.square == s.start).get.`type` == Pawn
      case _: Castle => false }
    history.size >= 100 && !history.takeRight(100).exists { case (m, b) => isPawnMoveOrCapture(m, b) }
  }
  lazy val mayClaimDraw = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] =
    movesToMake match {
      case h :: t => leaves.find { case (m, _) => h == m }.flatMap { case (_, b) => b.move(t) }
      case _ => Some(this)
    }

  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString() = "Board("+turn+","+pieces+")"

}
