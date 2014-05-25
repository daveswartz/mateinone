package mateinone

import Square._
import File._
import Rank._

object Board {

  def initial: Board = {
    def pieces(side: Side, second: File => Square, back: File => Square): Set[Piece] = {
      def piece(`type`: PieceType, rank: File => Square, files: Set[File]) = files.map(f => Piece(side, `type`, rank(f), hasMoved = false))
      piece(Pawn, second, files.toSet) ++ piece(Rook, back, Set(A, H)) ++ piece(Knight, back, Set(B, G)) ++ piece(Bishop, back, Set(C, F)) ++
        piece(King, back, Set(E)) ++ piece(Queen, back, Set(D))
    }
    Board(White, pieces(White, square(_, _2), square(_, _1)) ++ pieces(Black, square(_, _7), square(_, _8)))
  }

  def generateMoves(board: Board): Vector[MoveBase] = {

    val isWhite: Boolean = board.turn == White
    val occupied: Set[Square] = board.pieces.map(_.square)
    def isOpen(s: Square): Boolean = !occupied.contains(s)

    val movesAndPromotions = {

      val (up, upRight, right, downRight, down, downLeft, left, upLeft) = ((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1))
      val (file, rank) = (Vector(right, left), Vector(up, down))
      val diagonals = Vector(upRight, downRight, downLeft, upLeft)
      val adjacent = Vector(up, upRight, right, downRight, down, downLeft, left, upLeft)
      val knight = Vector((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))

      val opponents: Set[Square] = board.pieces.filter(_.side == board.turn.other).map(_.square)
      def isOpponent(s: Square): Boolean = opponents.contains(s)

      def steps(start: Square, step: (Int, Int)): Vector[Square] =
        Iterator.iterate[Option[Square]](Some(start))(_.flatMap(_ + step)).drop(1).takeWhile(_.isDefined).map(_.get).toVector

      def toMoves(start: Square, ends: Vector[Square]): Vector[MoveBase] = ends.map(e => Move.move(start, e))

      def pawnMoves(start: Square): Vector[MoveBase] = {

        val pawnAdvance = if (isWhite) up else down
        val advanceSteps = if (isWhite) { if (start.rank == `_2`) 2 else 1 } else { if (start.rank == `_7`) 2 else 1 }
        val pawnCaptures = if (isWhite) Vector(upLeft, upRight) else Vector(downLeft, downRight)
        val enPassants = Vector(left, right)
        val isPromotion = if (isWhite) start.rank == `_7` else start.rank == `_2`

        def open(step: (Int, Int), nSteps: Int): Vector[Square] = steps(start, step).take(nSteps).takeWhile(isOpen)
        def capture(step: (Int, Int)): Vector[Square] = steps(start, step).take(1).takeWhile(isOpponent)
        def enPassant(step: (Int, Int), captureStep: (Int, Int)): Vector[Square] =
          if (board.twoSquarePawnAdvance.exists(capture(captureStep).map(_.file) == Vector(_))) open(step, 1) else Vector()
        val ends = pawnCaptures.flatMap(capture) ++ open(pawnAdvance, advanceSteps) ++ pawnCaptures.zip(enPassants).flatMap { case (s, c) => enPassant(s, c) }
        def toPromotions(ends: Vector[Square]): Vector[MoveBase] = ends.flatMap(e => PromotionType.all.map(t => Promotion(start, e, t)))
        if (isPromotion) toPromotions(ends) else toMoves(start, ends)

      }

      def canCaptureMoves(start: Square, offsets: Vector[(Int, Int)]): Vector[MoveBase] =
        toMoves(start, offsets.flatMap(steps(start, _).span(isOpen) match { case (open, occ) => open ++ occ.take(1).takeWhile(isOpponent) }))
      def oneCanCaptureMove(start: Square, offsets: Vector[(Int, Int)]): Vector[MoveBase] =
        toMoves(start, offsets.flatMap(steps(start, _).take(1).takeWhile(s => isOpen(s) || isOpponent(s))))

      board.pieces filter(_.side == board.turn) flatMap {
        case Piece(_, Pawn, start, _) => pawnMoves(start)
        case Piece(_, Knight, start, _) => oneCanCaptureMove(start, knight)
        case Piece(_, Bishop, start, _) => canCaptureMoves(start, diagonals)
        case Piece(_, Rook, start, _) => canCaptureMoves(start, file ++ rank)
        case Piece(_, Queen, start, _) => canCaptureMoves(start, file ++ rank ++ diagonals)
        case Piece(_, King, start, _) => oneCanCaptureMove(start, adjacent)
      }

    }

    val castles: Vector[MoveBase] = {
      val stationary: Set[Square] = board.pieces.filter(!_.hasMoved).map(_.square)
      def hasMoved(s: Square): Boolean = !stationary.contains(s)
      val kingMoved = hasMoved(if (isWhite) E1 else E8)
      val kingside: Boolean = !hasMoved(if (isWhite) H1 else H8) && !kingMoved && (if (isWhite) Vector(F1, G1) else Vector(F8, G8)).forall(isOpen)
      val queenside: Boolean = !hasMoved(if (isWhite) A1 else A8) && !kingMoved && (if (isWhite) Vector(B1, C1, D1) else Vector(B8, C8, D8)).forall(isOpen)
      if (kingside && queenside) Vector(`O-O`, `O-O-O`) else if (kingside) Vector(`O-O`) else if (queenside) Vector(`O-O-O`) else Vector()
    }

    movesAndPromotions.toVector ++ castles

  }

  private type Position = (Set[Piece], Option[File])

  private def doMove(board: Board, move: MoveBase): Board = {

    def movePieces(ps: Set[Piece], start: Square, end: Square, `type`: PieceType) =
      ps.filterNot(_.square == start).filterNot(_.square == end) + Piece(board.turn, `type`, end, hasMoved = true)

    def createBoard(pieces: Set[Piece], last: MoveBase) = {
      val twoSquarePawnAdvance: Option[File] =
        last match {
          case Move(s, e) if pieces.find(_.square == e).get.`type` == Pawn && math.abs(e.rank.n - s.rank.n) == 2 => Some(e.file)
          case _ => None }
      val lastPawnMoveOrCapture = last match {
        case s: StartAndEnd if board.pieces.map(_.square).contains(s.end) || board.pieces.find(_.square == s.start).get.`type` == Pawn => 0
        case _ => board.lastPawnMoveOrCapture + 1 }
      Board(board.turn.other, pieces, lastPawnMoveOrCapture, board.positions :+ board.position, twoSquarePawnAdvance)
    }

    val oneMove: PartialFunction[MoveBase, Board] = { case m @ Move(start, end) =>
      createBoard(movePieces(board.pieces, start, end, board.pieces.find(_.square == start).get.`type`), m) }
    val onePromotion: PartialFunction[MoveBase, Board] = { case p @ Promotion(start, end, promotion) =>
      createBoard(movePieces(board.pieces, start, end, promotion), p) }
    val oneCastle: PartialFunction[MoveBase, Board] = { case c: Castle =>
      def doCastle(back: Rank) = {
        def moveKing(ps: Set[Piece], end: File) = movePieces(ps, square(E, back), square(end, back), King)
        def moveRook(ps: Set[Piece], start: File, end: File) = movePieces(ps, square(start, back), square(end, back), Rook)
        c match { case `O-O` => moveRook(moveKing(board.pieces, G), H, F); case `O-O-O` => moveRook(moveKing(board.pieces, C), A, D) } }
      createBoard(doCastle(if (board.turn == White) _1 else _8), c) }

    oneMove.orElse(onePromotion).orElse(oneCastle).apply(move)

  }

}
import Board._

case class Board private(turn: Side, pieces: Set[Piece],
                         private val lastPawnMoveOrCapture: Int = 0,
                         private val positions: Vector[Board.Position] = Vector(),
                         private val twoSquarePawnAdvance: Option[File] = None) {

  private lazy val legalAndIllegal: Vector[MoveBase] = generateMoves(this)

  private lazy val canCaptureKing: Boolean = {
    val opponentsKing = pieces.filter(p => p.`type` == King && p.side != turn).map(_.square)
    legalAndIllegal.exists { case s: StartAndEnd => opponentsKing.contains(s.end); case c: Castle => false }
  }

  private lazy val leaves: Map[MoveBase, Board] = {
    def castlesThroughCheck(between: Vector[Square]): Boolean =
      this.copy(pieces = pieces ++ between.map(Piece(turn, King, _, hasMoved = true))).isCheck
    legalAndIllegal
      .map(m => (m, doMove(this, m)))
      .filter(!_._2.canCaptureKing)
      .filter {
        case (`O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(F1) else Vector(F8))
        case (`O-O-O`, _) => !castlesThroughCheck(if (turn == White) Vector(C1, D1) else Vector(C8, D8))
        case _ => true }
      .toMap
  }

  private lazy val position: Position = (pieces, twoSquarePawnAdvance)

  def moves: Set[MoveBase] = leaves.keySet
  def boards: Iterable[Board] = leaves.values

  lazy val isCheck: Boolean = this.copy(turn = turn.other).canCaptureKing
  lazy val isCheckmate: Boolean = moves.isEmpty && isCheck

  lazy val isStalemate: Boolean = moves.isEmpty && !isCheck
  lazy val isInsufficientMaterial: Boolean = {
    val byType = pieces.groupBy(_.`type`)
    def isBlack(s: Square) = s.file.n % 2 == 0 && s.file.n % 2 == 0
    byType.keySet == Set(King) ||
      (byType.keySet == Set(King, Knight) && byType(Knight).size == 1) ||
      (byType.keySet == Set(King, Bishop) && byType(Bishop).map(b => isBlack(b.square)).size == 1)
  }
  lazy val isAutomaticDraw: Boolean = isStalemate || isInsufficientMaterial

  lazy val isThreefoldRepetition: Boolean = {
    def castlingRights(p: Position) = p._1.filter(p => p.`type` == King || p.`type` == Rook)
    def otherPieces(p: Position) = p._1.filter(p => p.`type` != King && p.`type` != Rook).map(p => (p.side, p.`type`, p.square))
    positions.count(other =>
      castlingRights(other) == castlingRights(position) && otherPieces(other) == otherPieces(position) && other._2 == position._2
    ) == 2
  }
  lazy val isFiftyMoveRule: Boolean = lastPawnMoveOrCapture >= 100
  lazy val mayClaimDraw: Boolean = isThreefoldRepetition || isFiftyMoveRule

  def move(movesToMake: List[MoveBase]): Option[Board] = movesToMake match { case h :: t => leaves.get(h).flatMap(_.move(t)); case _ => Some(this) }

  def move(movesToMake: MoveBase*): Option[Board] = move(movesToMake.toList)

  override def toString(): String = "Board("+turn+","+pieces+")"

}
