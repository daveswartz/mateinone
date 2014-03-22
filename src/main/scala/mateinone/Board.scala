package mateinone

import language.postfixOps
import Square._
import File._
import Rank._

object Board {

  val initial: Board = {
    def pieces(side: Side, second: File => Square, back: File => Square) = {
      def piece(`type`: PieceType, rank: File => Square, fs: File*) = fs.toVector.map(f => Piece(side, `type`, rank(f), false))
      piece(Pawn, second, files: _*) ++ piece(Rook, back, A, H) ++ piece(Knight, back, B, G) ++ piece(Bishop, back, C, F) ++
        piece(King, back, E) ++ piece(Queen, back, D)
    }
    Board(White, pieces(White, square(_, _2), square(_, _1)) ++ pieces(Black, square(_, _7), square(_, _8)), None)
  }

  private def between(c: Castle, s: Side): Vector[Square] = c match {
    case `O-O` => s match { case White => Vector(F1, G1); case Black => Vector(F8, G8) }
    case `O-O-O` => s match { case White => Vector(B1, C1, D1); case Black => Vector(B8, C8, D8) }
  }

  private def rook(c: Castle, s: Side): Square =
    c match { case `O-O` => s match { case White => H1; case Black => H8 }; case `O-O-O` => s match { case White => A1; case Black => A8 } }

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

    def enPassants(start: File, rankStep: Int) =
      board.pawnTwoSquareAdvanceEnd.map(end => Vector(1, -1).map(fileStep => if (start + fileStep == Some(end)) path((fileStep, rankStep)) else Vector())).getOrElse(Vector())

    def castles(side: Side): Vector[Castle] = {
      def canCastle(rook: Square, between: Vector[Square]) =
        board.pieces.exists(p => p.square == rook && !p.hasMoved) && between.forall(s => !board.pieces.exists(_.square == s))
      var result = Vector.empty[Castle]
      if (canCastle(rook(`O-O`, side), between(`O-O`, side))) result = result :+ `O-O`
      if (canCastle(rook(`O-O-O`, side), between(`O-O-O`, side))) result = result :+ `O-O-O`
      result
    }

    def toMoves(paths: Vector[Path]): Vector[Move] = paths.flatMap(_.map(end => Move(piece.square, end)))
    def toPromotions(paths: Vector[Path]): Vector[Promotion] = PromotionType.all.flatMap(t => paths.flatMap(_.map(end => Promotion(piece.square, end, t))))

    piece match {
      case Piece(White, Pawn,   Square(_, `_2`), false) => toMoves(pawnCaptures(White) :+ pawnAdvance((0, 1), 2))
      case Piece(White, Pawn,   Square(f, `_5`), true ) => toMoves(pawnCaptures(White) ++ enPassants(f, 1) :+ pawnAdvance((0, 1), 1))
      case Piece(White, Pawn,   Square(_, `_7`), true ) => toPromotions(pawnCaptures(White) :+ pawnAdvance((0, 1), 1))
      case Piece(White, Pawn,   _,               true ) => toMoves(pawnCaptures(White) :+ pawnAdvance((0, 1), 1))
      case Piece(Black, Pawn,   Square(_, `_7`), false) => toMoves(pawnCaptures(Black) :+ pawnAdvance((0, -1), 2))
      case Piece(Black, Pawn,   Square(f, `_4`), true ) => toMoves(pawnCaptures(Black) ++ enPassants(f, -1) :+ pawnAdvance((0, -1), 1))
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
      ps.filterNot(_.square == start).filterNot(_.square == end) :+ Piece(board.turn, `type`, end, true)
    val oneMove: PartialFunction[MoveBase, Board] =
    { case Move(start, end) =>
      val piece = board.pieces.find(_.square == start).get
      val pawnTwoSquareAdvanceEnd = if (piece.`type` == Pawn && math.abs(end.rank - start.rank) == 2) Some(end.file) else None
      Board(board.turn.other, movePieces(board.pieces, start, end, piece.`type`), pawnTwoSquareAdvanceEnd)
    }
    val onePromotion: PartialFunction[MoveBase, Board] =
    { case Promotion(start, end, promotion) => Board(board.turn.other, movePieces(board.pieces, start, end, promotion)) }
    val oneCastle: PartialFunction[MoveBase, Board] =
    { case c: Castle =>
      def doCastle(back: Rank) = {
        def moveKing(ps: Vector[Piece], end: File) = movePieces(ps, square(E, back), square(end, back), King)
        def moveRook(ps: Vector[Piece], start: File, end: File) = movePieces(ps, square(start, back), square(end, back), Rook)
        c match { case `O-O` => moveRook(moveKing(board.pieces, G), H, F); case `O-O-O` => moveRook(moveKing(board.pieces, C), A, D) }
      }
      Board(board.turn.other, doCastle(if (board.turn == White) _1 else _8))
    }
    oneMove.orElse(onePromotion).orElse(oneCastle).apply(move)
  }

}
import Board._

case class Board private(turn: Side, pieces: Vector[Piece], pawnTwoSquareAdvanceEnd: Option[File] = None) {

  private val legalAndIllegal: Vector[MoveBase] = pieces.filter(_.side == turn).flatMap(generateMoves(this, _))
  private val canCaptureKing = {
    val opponentsKing = pieces.find(p => p.side != turn && p.`type` == King).get.square
    legalAndIllegal.exists { case s: StartAndEnd => s.end == opponentsKing; case c: Castle => false }
  }

  def isCheck = Board(turn.other, pieces).canCaptureKing

  def isCheckmate = moves.isEmpty && isCheck

  def isStalemate = moves.isEmpty && !isCheck

  lazy val moves: Vector[MoveBase] = legalAndIllegal.filter { aMove =>
    val endsInCheck = { val next = doMove(this, aMove); next.canCaptureKing }
    val castlesThroughCheck = aMove match {
      case s: StartAndEnd => false
      case c: Castle =>
        def king(p: Piece) = p.side == turn && p.`type` == King
        val passesThroughCheck = between(c, turn).exists(b => Board(turn.other, pieces.filterNot(king) :+ Piece(turn, King, b, true)).canCaptureKing)
        isCheck || passesThroughCheck
    }
    !endsInCheck && !castlesThroughCheck
  }

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match {
    case h :: t => if (moves.contains(h)) doMove(this, h).move(t) else None
    case _ => Some(this)
  }

  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

}
