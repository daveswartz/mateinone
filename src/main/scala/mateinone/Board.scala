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

  private object generateMoves {

    private val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
    private val (file, rank) = (Vector(right, left), Vector(up, down))
    private val diagonals = Vector(upRight, downRight, downLeft, upLeft)
    private val adjacent = Vector(up, upRight, right, downRight, down, downLeft, left, upLeft)
    private val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
    private val enPassants = Vector(left, right)

    def apply(board: Board): Vector[MoveBase] = {

      val occupied: Set[Square] = board.pieces.map(_.square).toSet
      def isOpen(s: Square): Boolean = !occupied.contains(s)
      val opponents: Set[Square] = board.pieces.filter(_.side == board.turn.other).map(_.square).toSet
      def isOpponent(s: Square): Boolean = opponents.contains(s)

      val wasTwoSquarePawnAdvance: Boolean =
        board.history.lastOption match {
          case Some((Move(s, e), b)) if b.pieces.find(_.square == s).get.`type` == Pawn && math.abs(e.rank.n - s.rank.n) == 2 => true
          case _ => false }

      val (pawnCaptures, pawnAdvance) = if (board.turn == White) (Vector(upLeft, upRight), up) else (Vector(downLeft, downRight), down)

      board.pieces filter(_.side == board.turn) flatMap { piece =>

        def steps(step: (Int, Int)): Iterator[Square] =
          Iterator.iterate[Option[Square]](Some(piece.square))(_.flatMap(_ + step)).drop(1).takeWhile(_.isDefined).map(_.get)

        def toMoves(ends: Vector[Square]): Vector[MoveBase] = ends.map(e => Move(piece.square, e))

        def pawnMoves(p: Piece): Vector[MoveBase] = {
          def open(step: (Int, Int), nSteps: Int): Vector[Square] = steps(step).take(nSteps).takeWhile(isOpen).toVector
          def capture(step: (Int, Int)): Vector[Square] = steps(step).take(1).takeWhile(isOpponent).toVector
          def enPassant(step: (Int, Int), captureStep: (Int, Int)): Vector[Square] =
            if (wasTwoSquarePawnAdvance && !capture(captureStep).isEmpty) open(step, 1) else Vector()

          val (advanceSteps, isPromotion) =
            if (p.side == White) (if (p.square.rank == `_2`) 2 else 1, p.square.rank == `_7`)
            else                 (if (p.square.rank == `_7`) 2 else 1, p.square.rank == `_2`)

          val ends = pawnCaptures.flatMap(capture) ++ open(pawnAdvance, advanceSteps) ++ pawnCaptures.zip(enPassants).flatMap { case (s, c) => enPassant(s, c) }
          def toPromotions(ends: Vector[Square]): Vector[MoveBase] = ends.flatMap(e => PromotionType.all.map(t => Promotion(piece.square, e, t)))
          if (isPromotion) toPromotions(ends) else toMoves(ends)
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

        def canCaptureMoves(offsets: Vector[(Int, Int)]): Vector[MoveBase] =
          toMoves(offsets.flatMap(steps(_).span(isOpen) match { case (open, occ) => open ++ occ.take(1).takeWhile(isOpponent) }))
        def oneCanCaptureMove(offsets: Vector[(Int, Int)]): Vector[MoveBase] =
          toMoves(offsets.flatMap(steps(_).take(1).takeWhile(s => isOpen(s) || isOpponent(s))))

        piece match {
          case p @ Piece(_, Pawn, _, _)        => pawnMoves(p)
          case Piece(White, King,   E1, false) => oneCanCaptureMove(adjacent) ++ castles(White)
          case Piece(Black, King,   E8, false) => oneCanCaptureMove(adjacent) ++ castles(Black)
          case Piece(_,     Rook,    _, _    ) => canCaptureMoves(file ++ rank)
          case Piece(_,     Knight,  _, _    ) => oneCanCaptureMove(knight)
          case Piece(_,     Bishop,  _, _    ) => canCaptureMoves(diagonals)
          case Piece(_,     King,    _, _    ) => oneCanCaptureMove(adjacent)
          case Piece(_,     Queen,   _, _    ) => canCaptureMoves(file ++ rank ++ diagonals)
        }

      }
    }
  }

  private def between(c: Castle, s: Side): Vector[Square] = c match {
    case `O-O` => s match { case White => Vector(F1); case Black => Vector(F8) }
    case `O-O-O` => s match { case White => Vector(C1, D1); case Black => Vector(C8, D8) }
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

  private lazy val legalAndIllegal: Vector[MoveBase] = generateMoves(this)

  private lazy val canCaptureKing: Boolean = {
    val opponentsKing = pieces.filter(p => p.`type` == King && p.side != turn).map(_.square).toSet
    legalAndIllegal.exists { case s: StartAndEnd => opponentsKing.contains(s.end); case c: Castle => false }
  }

  private lazy val leaves: Map[MoveBase, Board] = {
    def castlesThroughCheck(m: MoveBase): Boolean =
      m match {
        case s: StartAndEnd => false
        case c: Castle => this.copy(pieces = pieces ++ between(c, turn).map(s => Piece(turn, King, s, hasMoved = true))).isCheck
      }
    legalAndIllegal.map(m => (m, doMove(this, m))).filter { case (m, b) => !castlesThroughCheck(m) && !b.canCaptureKing }.toMap
  }

  lazy val moves: Set[MoveBase] = leaves.keySet
  lazy val boards: Iterable[Board] = leaves.values

  lazy val isCheck: Boolean = this.copy(turn = turn.other).canCaptureKing
  lazy val isCheckmate: Boolean = moves.isEmpty && isCheck

  lazy val isStalemate: Boolean = moves.isEmpty && !isCheck
  lazy val isInsufficientMaterial: Boolean = {
    val byType = pieces.groupBy(_.`type`)
    def isBlack(s: Square) = s.file.n % 2 == 0 && s.file.n % 2 == 0
    byType.keySet == Set(King) ||
      (byType.keySet == Set(King, Knight) && byType(Knight).size == 1) ||
      (byType.keySet == Set(King, Bishop) && byType(Bishop).map(b => isBlack(b.square)).distinct.size == 1)
  }
  lazy val isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  lazy val isThreefoldRepetition: Boolean = history.groupBy(_._2.moves).exists { case (_, repeats) => repeats.size == 3 }
  lazy val isFiftyMoveRule: Boolean = {
    def isPawnMoveOrCapture(m: MoveBase, b: Board) = m match {
      case s: StartAndEnd => b.pieces.map(_.square).contains(s.end) || b.pieces.find(_.square == s.start).get.`type` == Pawn
      case _: Castle => false }
    history.size >= 100 && !history.takeRight(100).exists { case (m, b) => isPawnMoveOrCapture(m, b) }
  }
  lazy val mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match { case h :: t => leaves.get(h).flatMap(_.move(t)); case _ => Some(this) }

  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString(): String = "Board("+turn+","+pieces+")"

}
