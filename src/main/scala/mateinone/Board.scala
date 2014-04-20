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

  private def generateMoves(board: Board, piece: Piece): Vector[MoveBase] = {

    type Path = Vector[Square]

    def path(stepOffset: (Int, Int), nSteps: Int = 1, stepOnOther: Boolean = true, stepOnEmpty: Boolean = true): Path = {
      def pathRecur(current: Path, remaining: Int): Path = {
        if (remaining == 0) current
        else {
          current.last + stepOffset match {
            case None =>
              current
            case Some(next) if board.pieces.exists(p => p.square == next && p.side != piece.side) =>
              if (stepOnOther) current :+ next else current
            case Some(next) if board.pieces.exists(p => p.square == next && p.side == piece.side) =>
              current
            case Some(next) =>
              if (stepOnEmpty) pathRecur(current :+ next, remaining - 1) else current
          }
        }
      }
      pathRecur(Vector(piece.square), nSteps).tail
    }

    def file = Vector(path((1, 0), 7), path((-1, 0), 7))
    def rank = Vector(path((0, 1), 7), path((0, -1), 7))
    def diagonals = Vector(path((1, 1), 7), path((1, -1), 7), path((-1, 1), 7), path((-1, -1), 7))
    def adjacent = Vector(path((0, 1)), path((1, 1)), path((1, 0)), path((1, -1)), path((0, -1)), path((-1, -1)), path((-1, 0)), path((-1, 1)))

    def pawnAdvance(s: (Int, Int), n: Int) = path(s, n, stepOnOther = false)

    def pawnCaptures(side: Side) = {
      val one = path(_: (Int, Int), 1, stepOnEmpty = false)
      if (side == White) Vector(one(1, 1), one(-1, 1)) else Vector(one(1, -1), one(-1, -1))
    }

    def enPassant(start: File, rankStep: Int): Option[Path] = {
      val (lastBoard, lastMove) = board.history.last
      def isAllowed(last: Move, fileStep: Int) = {
        val isPawn = lastBoard.pieces.find(_.square == last.start).get.`type` == Pawn
        val isTwoSquareAdvance = math.abs(last.end.rank - last.start.rank) == 2
        val canCapture = start + fileStep == Some(last.end.file)
        isPawn && isTwoSquareAdvance && canCapture
      }
      lastMove match {
        case last: Move if isAllowed(last, 1) => Some(path((1, rankStep)))
        case last: Move if isAllowed(last, -1) => Some(path((-1, rankStep)))
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

    def toMoves(paths: Vector[Path]): Vector[Move] = paths.flatMap(_.map(end => Move(piece.square, end)))
    def toPromotions(paths: Vector[Path]): Vector[Promotion] = PromotionType.all.flatMap(t => paths.flatMap(_.map(end => Promotion(piece.square, end, t))))

    piece match {
      case Piece(White, Pawn,   Square(_, `_2`), false) => toMoves(pawnCaptures(White) :+ pawnAdvance((0, 1), 2))
      case Piece(White, Pawn,   Square(f, `_5`), true ) => toMoves(pawnCaptures(White) ++ enPassant(f, 1) :+ pawnAdvance((0, 1), 1))
      case Piece(White, Pawn,   Square(_, `_7`), true ) => toPromotions(pawnCaptures(White) :+ pawnAdvance((0, 1), 1))
      case Piece(White, Pawn,   _,               true ) => toMoves(pawnCaptures(White) :+ pawnAdvance((0, 1), 1))
      case Piece(Black, Pawn,   Square(_, `_7`), false) => toMoves(pawnCaptures(Black) :+ pawnAdvance((0, -1), 2))
      case Piece(Black, Pawn,   Square(f, `_4`), true ) => toMoves(pawnCaptures(Black) ++ enPassant(f, -1) :+ pawnAdvance((0, -1), 1))
      case Piece(Black, Pawn,   Square(_, `_2`), true ) => toPromotions(pawnCaptures(Black) :+ pawnAdvance((0, -1), 1))
      case Piece(Black, Pawn,   _,               true ) => toMoves(pawnCaptures(Black) :+ pawnAdvance((0, -1), 1))
      case Piece(_,     Rook,   _,               _    ) => toMoves(file ++ rank)
      case Piece(_,     Knight, _,               _    ) => toMoves(Vector(path((2, 1)), path((2, -1)), path((1, 2)), path((1, -2)), path((-2, 1)), path((-2, -1)), path((-1, 2)), path((-1, -2))))
      case Piece(_,     Bishop, _,               _    ) => toMoves(diagonals)
      case Piece(White, King,   E1,              false) => toMoves(adjacent) ++ castles(White)
      case Piece(Black, King,   E8,              false) => toMoves(adjacent) ++ castles(Black)
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

case class Board private(turn: Side, pieces: Vector[Piece], history: Vector[(Board, MoveBase)]) {

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

  def moves: Vector[MoveBase] =
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

}
