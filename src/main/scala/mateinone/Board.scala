package mateinone

import Square._
import File._
import Rank._

object Board {

  val initial: Board = {
    def pieces(side: Side, second: File => Square, back: File => Square) = {
      def piece(`type`: PieceType, rank: File => Square, fs: File*) = fs.toVector.map(f => Piece(side, `type`, rank(f), hasMoved = false))
      piece(Pawn, second, files: _*) ++ piece(Rook, back, A, H) ++ piece(Knight, back, B, G) ++ piece(Bishop, back, C, F) ++
        piece(King, back, E) ++ piece(Queen, back, D)
    }
    Board(White, pieces(White, square(_, _2), square(_, _1)) ++ pieces(Black, square(_, _7), square(_, _8)), Vector())
  }

  private def between(c: Castle, s: Side): Vector[Square] = c match {
    case `O-O` => s match { case White => Vector(F1, G1); case Black => Vector(F8, G8) }
    case `O-O-O` => s match { case White => Vector(B1, C1, D1); case Black => Vector(B8, C8, D8) }
  }

  private def generateMoves(board: Board, piece: Piece): Iterable[MoveBase] = {

    def isOccupied(square: Square) = board.pieces.exists(p => p.square == square)
    def isCapture(square: Square) = board.pieces.exists(p => p.square == square && p.side == piece.side.other)
    def steps(step: (Int, Int)) = Iterator.iterate[Option[Square]](Some(piece.square))(_.flatMap(_ + step)).drop(1).takeWhile(_.isDefined).map(_.get)
    def pawnAdvance(step: (Int, Int), nSteps: Int) = steps(step).take(nSteps).takeWhile(!isOccupied(_))
    def pawnCapture(step: (Int, Int)) = steps(step).take(1).takeWhile(isCapture)
    def canCapture(step: (Int, Int)) = steps(step).span(!isOccupied(_)) match { case (empty, occ) => empty ++ occ.take(1).takeWhile(isCapture) }
    def file = List((1, 0), (-1, 0)).map(canCapture)
    def rank = List((0, 1), (0, -1)).map(canCapture)
    def diagonals = List((1, 1), (1, -1), (-1, 1), (-1, -1)).map(canCapture)
    def adjacent = List((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)).map(canCapture(_).take(1))

    def enPassant(start: File, rankStep: Int): Option[Iterator[Square]] = {
      val (lastBoard, lastMove) = board.history.last
      def isAllowed(last: Move, fileStep: Int) = {
        val isPawn = lastBoard.pieces.find(_.square == last.start).get.`type` == Pawn
        val isTwoSquareAdvance = math.abs(last.end.rank - last.start.rank) == 2
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
      case Piece(White, Pawn,   Square(_, `_2`), false) => toMoves(Iterable(pawnCapture(1, 1), pawnCapture(-1, 1), pawnAdvance((0, 1), 2)))
      case Piece(White, Pawn,   Square(f, `_5`), true ) => toMoves(Iterable(pawnCapture(1, 1), pawnCapture(-1, 1), pawnAdvance((0, 1), 1)) ++ enPassant(f, 1))
      case Piece(White, Pawn,   Square(_, `_7`), true ) => toPromotions(Iterable(pawnCapture(1, 1), pawnCapture(-1, 1), pawnAdvance((0, 1), 1)))
      case Piece(White, Pawn,   _,               true ) => toMoves(Iterable(pawnCapture(1, 1), pawnCapture(-1, 1), pawnAdvance((0, 1), 1)))
      case Piece(White, King,   E1,              false) => toMoves(adjacent) ++ castles(White)
      case Piece(Black, Pawn,   Square(_, `_7`), false) => toMoves(Iterable(pawnCapture(1, -1), pawnCapture(-1, -1), pawnAdvance((0, -1), 2)))
      case Piece(Black, Pawn,   Square(f, `_4`), true ) => toMoves(Iterable(pawnCapture(1, -1), pawnCapture(-1, -1), pawnAdvance((0, -1), 1)) ++ enPassant(f, -1))
      case Piece(Black, Pawn,   Square(_, `_2`), true ) => toPromotions(Iterable(pawnCapture(1, -1), pawnCapture(-1, -1), pawnAdvance((0, -1), 1)))
      case Piece(Black, Pawn,   _,               true ) => toMoves(Iterable(pawnCapture(1, -1), pawnCapture(-1, -1), pawnAdvance((0, -1), 1)))
      case Piece(Black, King,   E8,              false) => toMoves(adjacent) ++ castles(Black)
      case Piece(_,     Rook,   _,               _    ) => toMoves(file ++ rank)
      case Piece(_,     Knight, _,               _    ) => toMoves(Iterable((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2)).map(canCapture(_).take(1)))
      case Piece(_,     Bishop, _,               _    ) => toMoves(diagonals)
      case Piece(_,     King,   _,               _    ) => toMoves(adjacent)
      case Piece(_,     Queen,  _,               _    ) => toMoves(file ++ rank ++ diagonals)
    }

  }

  private def doMove(board: Board, move: MoveBase): Board = {
    def movePieces(ps: Vector[Piece], start: Square, end: Square, `type`: PieceType) =
      ps.filterNot(_.square == start).filterNot(_.square == end) :+ Piece(board.turn, `type`, end, hasMoved = true)
    def createBoard(pieces: Vector[Piece], last: MoveBase) = Board(board.turn.other, pieces, board.history :+ (board, move))
    val oneMove: PartialFunction[MoveBase, Board] =
      { case m @ Move(start, end) =>
        createBoard(movePieces(board.pieces, start, end, board.pieces.find(_.square == start).get.`type`), m) }
    val onePromotion: PartialFunction[MoveBase, Board] =
      { case p @ Promotion(start, end, promotion) =>
        createBoard(movePieces(board.pieces, start, end, promotion), p) }
    val oneCastle: PartialFunction[MoveBase, Board] =
      { case c: Castle =>
        def doCastle(back: Rank) = {
          def moveKing(ps: Vector[Piece], end: File) = movePieces(ps, square(E, back), square(end, back), King)
          def moveRook(ps: Vector[Piece], start: File, end: File) = movePieces(ps, square(start, back), square(end, back), Rook)
          c match { case `O-O` => moveRook(moveKing(board.pieces, G), H, F); case `O-O-O` => moveRook(moveKing(board.pieces, C), A, D) } }
        createBoard(doCastle(if (board.turn == White) _1 else _8), c) }
    oneMove.orElse(onePromotion).orElse(oneCastle).apply(move)
  }

}
import Board._

case class Board private(turn: Side, pieces: Vector[Piece], private val history: Vector[(Board, MoveBase)]) {

  private def legalAndIllegal: Vector[MoveBase] = pieces.filter(_.side == turn).flatMap(generateMoves(this, _))

  private def canCaptureKing = {
    val opponentsKing = pieces.find(p => p.side != turn && p.`type` == King).get.square
    legalAndIllegal.exists { case s: StartAndEnd => s.end == opponentsKing; case c: Castle => false }
  }

  def isCheck = this.copy(turn = turn.other).canCaptureKing
  def isCheckmate = moves.isEmpty && isCheck

  def isStalemate = moves.isEmpty && !isCheck
  def isInsufficientMaterial = {
    val byType = pieces.groupBy(_.`type`)
    def isBlack(s: Square) = s.file.n % 2 == 0 && s.file.n % 2 == 0
    byType.keySet == Set(King) ||
      (byType.keySet == Set(King, Knight) && byType(Knight).size == 1) ||
      (byType.keySet == Set(King, Bishop) && byType(Bishop).map(b => isBlack(b.square)).distinct.size == 1)
  }
  def isAutomaticDraw = isStalemate || isInsufficientMaterial

  def isThreefoldRepetition = history.groupBy(_._1.moves).exists { case (_, repeats) => repeats.size == 3 }
  def isFiftyMoveRule = {
    def isPawnMoveOrCapture(b: Board, m: MoveBase) = m match {
      case s: StartAndEnd => b.pieces.map(_.square).contains(s.end) || b.pieces.find(_.square == s.start).get.`type` == Pawn
      case _: Castle => false }
    history.size >= 100 && !history.takeRight(100).exists { case (b, m) => isPawnMoveOrCapture(b, m) }
  }
  def mayClaimDraw = isThreefoldRepetition || isFiftyMoveRule

  lazy val moves: Vector[MoveBase] =
    legalAndIllegal.filter { aMove =>
      val endsInCheck = { val next = doMove(this, aMove); next.canCaptureKing }
      val castlesThroughCheck = aMove match {
        case s: StartAndEnd => false
        case c: Castle =>
          def king(p: Piece) = p.side == turn && p.`type` == King
          val passesThroughCheck = between(c, turn).exists(b => this.copy(turn = turn.other, pieces = pieces.filterNot(king) :+ Piece(turn, King, b, hasMoved = true)).canCaptureKing)
          isCheck || passesThroughCheck
      }
      !endsInCheck && !castlesThroughCheck
    }

  def move(movesToMake: List[MoveBase]): Option[Board] =
    movesToMake match {
      case h :: t => if (moves.contains(h)) doMove(this, h).move(t) else None
      case _ => Some(this)
    }

  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString() = "Board("+turn+","+pieces+")"

}
