package mateinone

import OccupiedPath._
import Move._

// TODO change to generate all possible moves, check if a move is valid by checking against the list of moves
object Board {

  // Generates moves for all configuration of the board constraining only the specified piece to its square.  Returns
  // the union of the generated end squares for the piece's moves.
  private def pathsFor(piece: Piece): Set[Path] = {

    def path(square: Square, fileOffset: Int, rankOffset: Int, nSteps: Int): Path = {
      def pathRecur(current: Path, remaining: Int): Path =
        Square.offset(current.last, fileOffset, rankOffset) match {
          case Some(next) if remaining > 0 => pathRecur(current :+ next, remaining - 1)
          case _ => current
        }
      pathRecur(List(square), nSteps).tail
    }

    def single(s: Square)(offset: (Int, Int)) = offset match { case (f, r) => path(s, f, r, 1) }
    def file(s: Square) = Set(path(s, 1, 0, 7), path(s, -1, 0, 7))
    def rank(s: Square) = Set(path(s, 0, 1, 7), path(s, 0, -1, 7))
    def diagonals(s: Square) = Set(path(s, 1, 1, 7), path(s, 1, -1, 7), path(s, -1, 1, 7), path(s, -1, -1, 7))
    def adjacent(s: Square) = Set((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)).map(single(s))

    val paths = piece match {
      case Piece(White, Pawn, square) =>
        Set(path(square, 0, 1, if (square.rank == `2`) 2 else 1), path(square, 1, 1, 1), path(square, -1, 1, 1))
      case Piece(Black, Pawn, square) =>
        Set(path(square, 0, -1, if (square.rank == `7`) 2 else 1), path(square, 1, -1, 1), path(square, -1, -1, 1))
      case Piece(_, Rook, square) =>
        file(square) ++ rank(square)
      case Piece(_, Knight, square) =>
        Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2)).map(single(square))
      case Piece(_, Bishop, square) =>
        diagonals(square)
      case Piece(White, King, Square.e1) =>
        adjacent(Square.e1) ++ Set((2, 0), (-2, 0)).map(single(Square.e1))
      case Piece(Black, King, Square.e8) =>
        adjacent(Square.e8) ++ Set((2, 0), (-2, 0)).map(single(Square.e8))
      case Piece(_, King, square) =>
        adjacent(square)
      case Piece(_, Queen, square) =>
        file(square) ++ rank(square) ++ diagonals(square)
    }

    paths.filterNot(_.isEmpty)

  }

  // Marks the squares in the path which are occupied by other pieces.
  private def occupiedPathsFor(piece: Piece, otherPieces: Set[Piece]): Set[OccupiedPath] = {
    def occupied(path: Path) = otherPieces.map(_.square).filter(path.contains)
    pathsFor(piece).map(path => OccupiedPath(path, occupied(path)))
  }

  // Creates a chess board in the initial state
  def apply(): Board = {
    def piecesFor(side: Side, pawnRank: Rank, kingRank: Rank): Set[Piece] = {
      def piece(side: Side, pieceType: PieceType)(square: Square) = Piece(side, pieceType, square)
      val pawns = File.allFiles.map(Square.get(_, pawnRank)).toSet.map(piece(side, Pawn))
      val rooks = Set(A, H).map(Square.get(_, kingRank)).map(piece(side, Rook))
      val knights = Set(B, G).map(Square.get(_, kingRank)).map(piece(side, Knight))
      val bishops = Set(C, F).map(Square.get(_, kingRank)).map(piece(side, Bishop))
      val king = piece(side, King)(Square.get(E, kingRank))
      val queen = piece(side, Queen)(Square.get(D, kingRank))
      pawns ++ rooks ++ knights ++ bishops + king + queen
    }
    Board(White, Nil, piecesFor(White, `2`, `1`) ++ piecesFor(Black, `7`, `8`))
  }

}
import Board._

case class Board private(turn: Side, history: List[Move], pieces: Set[Piece]) {

  def pieceAt(square: Square): Option[Piece] = pieces.find(_.square == square)

  private def lastMove(piece: Piece): Option[Move] = history.find {
    case SimpleMove(_, end) => end == piece.square
    case Promotion(_, end, _) => end == piece.square
    case Castle(_, end, SimpleMove(_, rookEnd)) => end == piece.square || rookEnd == piece.square
  }

  def hasMoved(piece: Piece): Boolean = lastMove(piece).isDefined

  private def occupiedPathEnds(occupiedPath: OccupiedPath): List[Square] = // TODO pull into occupied path class, passing in turn
    occupiedPath.beforeFirstOccupied ++ occupiedPath.firstOccupied.filter(pieceAt(_).fold(false)(_.side != turn))

  // Returns true when the move is valid; otherwise, false.
  def isValid(move: Move): Boolean = {

    def ends(piece: Piece): Set[Square] = occupiedPathsFor(piece, pieces - piece).flatMap(occupiedPathEnds)

    def isValidCastle(castle: Castle, piece: Piece) =
      pieceAt(castle.rookMove.start).fold(false)(rookPiece => !hasMoved(piece) && !hasMoved(rookPiece) && ends(rookPiece).contains(castle.rookMove.end))

    def isValidSimpleMove(simpleMove: SimpleMove, piece: Piece) =
      simpleMove match { case SimpleMove(start @ Square(file, rank), end) =>
        piece match { case Piece(side, pieceType, _) =>

          val mustBeCastle = pieceType == King && math.abs(File.offset(file, end.file)) == 2

          val isInvalidPawnMove = if (pieceType == Pawn) {
            val invalidPromotion = (side == White && rank == `7`) || (side == Black && rank == `2`)
            val fileOffset = File.offset(file, end.file)
            val diagonal = math.abs(fileOffset) == 1
            val diagonalWhenNotCapturing = {
              val enPassant = history.lastOption match {
                case Some(SimpleMove(lastStart, lastEnd)) =>
                  val lastPiece = pieceAt(lastEnd).get
                  val isPawn = lastPiece.pieceType == Pawn
                  val wasTwoSquareAdvance = Square.offset(lastStart, lastEnd) == (if (lastPiece.side == White) (0, 2) else (0, -2))
                  val isInTargetSquare = lastEnd == Square.offset(start, fileOffset, 0).get // TODO only check in adj file
                  isPawn && wasTwoSquareAdvance && isInTargetSquare
                case _ => false
              }
              val otherCapture = pieceAt(end).fold(false)(_.side != side)
              diagonal && !enPassant && !otherCapture
            }
            val nonDiagonalCapture = !diagonal && pieceAt(end).isDefined
            invalidPromotion || diagonalWhenNotCapturing || nonDiagonalCapture
          } else false

          !mustBeCastle && !isInvalidPawnMove

        }
      }

    def isBlockingCheck = {
      val kingSquare = pieces.find(p => p.pieceType == King && p.side == turn).get.square
      val opposingOccupiedPaths = pieces.filter(_.side != turn).flatMap(piece => occupiedPathsFor(piece, pieces - piece))
      def accept(op: OccupiedPath) = op.firstOccupied.fold(false)(move.start ==) && op.secondOccupied.fold(false)(kingSquare ==)
      opposingOccupiedPaths.exists(accept)
    }

    pieceAt(move.start).fold(false) { piece =>
      piece.side == turn &&
        ends(piece).contains(move.end) &&
        !isBlockingCheck &&
        (move match {
          case castle: Castle => isValidCastle(castle, piece)
          case promotion: Promotion => true
          case simpleMove: SimpleMove => isValidSimpleMove(simpleMove, piece)
        })
    }

  }

  // Returns `Some[Board]` when the moves are valid; otherwise, `None`. The repeated parameter is either a `Move`
  // instance or a function that takes a `Side` and returns a `Move`. The reason for using an `Either` is to allow
  // castling to be specified as `O-O` or `O-O-O` without requiring the side to be specified.
  def move(moves: Either[Move, Side => Move]*): Option[Board] = {

    def oneMove(move: Move): Option[Board] =
      if (isValid(move)) {
        val nextTurn = if (turn == White) Black else White
        val nextHistory = history :+ move
        pieceAt(move.start).flatMap { piece =>
          move match {
            case SimpleMove(_, end) =>
              Some(Board(nextTurn, nextHistory, pieces.filterNot(_.square == end) - piece + piece.copy(square = end)))
            case Castle(_, end, SimpleMove(rookStart, rookEnd)) =>
              pieceAt(rookStart).flatMap { rookPiece =>
                Some(Board(nextTurn, nextHistory, pieces - piece - rookPiece + piece.copy(square = end) + rookPiece.copy(square = rookEnd)))
              }
            case Promotion(_, end, promotionType) =>
              Some(Board(nextTurn, nextHistory, pieces.filterNot(_.square == end) - piece + piece.copy(square = end).promotedTo(promotionType)))
          }
        }
      } else None

    moves.toList match {
      case head :: tail => oneMove(toMove(head, turn)).flatMap(_.move(tail :_*))
      case Nil => Some(this)
    }

  }

  // Returns the set of valid moves.
  def moves: Set[Move] =
    pieces.flatMap(piece => occupiedPathsFor(piece, pieces - piece).flatMap(occupiedPathEnds(_).flatMap(Move.moves(piece.square, _).iterator))).filter(isValid)

}
