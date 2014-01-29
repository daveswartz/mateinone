package mateinone

import OccupiedPath._
import Move._

object Board {

  // Generates an indexed sequence of squares for the specified piece corresponding to all possible moves the piece can
  // make.
  private def pathsFor(piece: Piece): Set[Path] = {

    def path(square: Square, fileOffset: Int, rankOffset: Int, nSteps: Int): Path = {
      def pathRecur(current: Path, remaining: Int): Path =
        Square.offset(current.last, fileOffset, rankOffset) match {
          case Some(next) if remaining > 0 => pathRecur(current :+ next, remaining - 1)
          case _ => current
        }
      pathRecur(List(square), nSteps).tail
    }

    def file(s: Square) = Set(path(s, 1, 0, 7), path(s, -1, 0, 7))
    def rank(s: Square) = Set(path(s, 0, 1, 7), path(s, 0, -1, 7))
    def diagonals(s: Square) = Set(path(s, 1, 1, 7), path(s, 1, -1, 7), path(s, -1, 1, 7), path(s, -1, -1, 7))

    val paths = piece match {
      case Piece(White, Pawn, square) =>
        Set(path(square, 0, 1, if (square.rank == `2`) 2 else 1), path(square, 1, 1, 1), path(square, -1, 1, 1))
      case Piece(Black, Pawn, square) =>
        Set(path(square, 0, -1, if (square.rank == `7`) 2 else 1), path(square, 1, -1, 1), path(square, -1, -1, 1))
      case Piece(_, Rook, square) =>
        file(square) ++ rank(square)
      case Piece(_, Knight, square) =>
        Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2)).map { case (f, r) => path(square, f, r, 1) }
      case Piece(_, Bishop, square) =>
        diagonals(square)
      case Piece(_, King, square) =>
        Set((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (2, 0), (-2, 0)).map { case (f, r) => path(square, f, r, 1) }
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

    def piece(side: Side, pieceType: PieceType)(square: Square) = Piece(side, pieceType, square)

    def piecesFor(side: Side, pawnRank: Rank, kingRank: Rank): Set[Piece] = {
      val pawns = File.allFiles.map(Square.get(_, pawnRank)).toSet.map(piece(side, Pawn))
      val rooks = Set(A, H).map(Square.get(_, kingRank)).map(piece(side, Rook))
      val knights = Set(B, G).map(Square.get(_, kingRank)).map(piece(side, Knight))
      val bishops = Set(C, F).map(Square.get(_, kingRank)).map(piece(side, Bishop))
      val king = piece(side, King)(Square.get(E, kingRank))
      val queen = piece(side, Queen)(Square.get(D, kingRank))
      pawns ++ rooks ++ knights ++ bishops + king + queen
    }

    val initialPieces = piecesFor(White, `2`, `1`) ++ piecesFor(Black, `7`, `8`)
    new Board {
      val turn = White
      val history = Nil
      val piecesToOccupiedPaths = initialPieces.map(piece => (piece, occupiedPathsFor(piece, initialPieces - piece))).toMap
    }

  }

}
import Board._

trait Board {

  val turn: Side

  protected val history: List[Move]

  private def lastMove(piece: Piece): Option[Move] = history.find {
    case SimpleMove(_, end) => end == piece.square
    case Promotion(_, end, _) => end == piece.square
    case Castle(_, end, SimpleMove(_, rookEnd)) => end == piece.square || rookEnd == piece.square
  }

  def hasMoved(piece: Piece): Boolean = lastMove(piece).isDefined

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keySet

  def pieceAt(square: Square): Option[Piece] = pieces.find(_.square == square)

  private def occupiedPathEnds(occupiedPath: OccupiedPath): List[Square] =
    occupiedPath.beforeFirstOccupied ++ occupiedPath.firstOccupied.filter(pieceAt(_).fold(false)(_.side != turn))

  // Returns true when the move is valid; otherwise, false.
  def isValid(move: Move): Boolean = {

    def ends(piece: Piece): Set[Square] = piecesToOccupiedPaths(piece).flatMap(occupiedPathEnds)

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
                  val isInTargetSquare = lastEnd == Square.offset(start, fileOffset, 0).get
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
      val opposingOccupiedPaths = piecesToOccupiedPaths.filterKeys(_.side != turn).values.flatten.toSet
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

    def oneMove(move: Move): Option[Board] = {

      def updateOccupiedOf(stationary: Set[Piece], moves: Set[Move]) =
        stationary.map(p => (p, piecesToOccupiedPaths(p).map(_.vacate(moves.map(_.start)).occupy(moves.map(_.end))))).toMap

      def updatePathsOf(movedBefore: Set[Piece], movedAfter: Set[Piece]) =
        movedAfter.map(p => (p, occupiedPathsFor(p, pieces -- movedBefore))).toMap


      def doMove(movesMade: Set[Move], movedBefore: Set[Piece], movedAfter: Set[Piece], stationary: Set[Piece]) = {
        val nextTurn = if (turn == White) Black else White
        val nextHistory = history :+ move
        val captured = movedAfter.map(_.square).flatMap(pieceAt)
        val nextPiecesToOccupiedPaths = (updateOccupiedOf(stationary, movesMade) ++ updatePathsOf(movedBefore, movedAfter)).filterKeys(!captured.contains(_))
        Some(new Board {
          val turn = nextTurn
          val history = nextHistory
          val piecesToOccupiedPaths = nextPiecesToOccupiedPaths
        })
      }

      pieceAt(move.start).flatMap { piece =>
        if (isValid(move)) {
          move match {
            case SimpleMove(_, end) =>
              doMove(Set(move), Set(piece), Set(piece.atEnd(move)), pieces - piece)
            case Castle(_, _, rookMove @ SimpleMove(rookStart, rookEnd)) =>
              pieceAt(rookStart).flatMap { rookPiece =>
                doMove(Set(move, rookMove), Set(piece), Set(piece.atEnd(move), rookPiece.atEnd(rookMove)), pieces - piece - rookPiece)
              }
            case Promotion(_, end, promotionType) =>
              doMove(Set(move), Set(piece), Set(piece.atEnd(move).promotedTo(promotionType)), pieces - piece)
          }
        } else None
      }
    }

    moves.toList match {
      case head :: tail => oneMove(toMove(head, turn)).flatMap(_.move(tail :_*))
      case Nil => Some(this)
    }

  }

  // Returns the set of valid moves.
  def moves: Set[Move] = {
    def movesFor(piece: Piece, end: Square): Set[Move] = {
      val promotions: Set[Move] = PromotionType.all.flatMap(Promotion.optionally(piece.square, end, _))
      val castles: Set[Move] = Castle.all.map(_.rookMove).flatMap(Castle.optionally(piece.square, end, _))
      val simple: Move = SimpleMove(piece.square, end)
      promotions ++ castles + simple
    }
    piecesToOccupiedPaths.flatMap { case (k, v) => v.flatMap(occupiedPathEnds(_).flatMap(movesFor(k, _))) }.filter(isValid).toSet
  }

}
