package mateinone

import Move._

// TODO change to generate all possible moves, check if a move is valid by checking against the list of moves
// TODO add lastMove: Option[Move] to each piece and val hasMoved and twoSquarePawnAdvance
object Board {

  private def paths(piece: Piece, others: Set[Piece]): Set[List[Square]] = {

    def path(fileOffset: Int, rankOffset: Int, nSteps: Int = 1): List[Square] = {
      def pathRecur(current: List[Square], remaining: Int): List[Square] = {
        def otherWith(side: Side, square: Square) = others.exists(o => o.side == side && o.square == square)
        Square.offset(current.last, fileOffset, rankOffset) match {
          case Some(next) if otherWith(piece.side, next) =>
            current
          case Some(next) if otherWith(piece.side.other, next) =>
            current :+ next
          case Some(next) if remaining > 0 =>
            pathRecur(current :+ next, remaining - 1)
          case _ =>
            current
        }
      }
      pathRecur(List(piece.square), nSteps).tail
    }

    def file = Set(path(1, 0, 7), path(-1, 0, 7))
    def rank = Set(path(0, 1, 7), path(0, -1, 7))
    def diagonals = Set(path(1, 1, 7), path(1, -1, 7), path(-1, 1, 7), path(-1, -1, 7))
    def adjacent = Set(path(0, 1), path(1, 1), path(1, 0), path(1, -1), path(0, -1), path(-1, -1), path(-1, 0), path(-1, 1))

    piece match {
      case Piece(White, Pawn, Square(_, `2`)) =>
        Set(path(0, 1, 2), path(1, 1, 1), path(-1, 1, 1))
      case Piece(White, Pawn, _) =>
        Set(path(0, 1, 1), path(1, 1, 1), path(-1, 1, 1))
      case Piece(Black, Pawn, Square(_, `7`)) =>
        Set(path(0, -1, 2), path(1, -1, 1), path(-1, -1, 1))
      case Piece(Black, Pawn, _) =>
        Set(path(0, -1, 1), path(1, -1, 1), path(-1, -1, 1))
      case Piece(_, Rook, _) =>
        file ++ rank
      case Piece(_, Knight, _) =>
        Set(path(2, 1), path(2, -1), path(1, 2), path(1, -2), path(-2, 1), path(-2, -1), path(-1, 2), path(-1, -2))
      case Piece(_, Bishop, _) =>
        diagonals
      case Piece(White, King, Square.e1) =>
        adjacent ++ Set(path(2, 0), path(-2, 0))
      case Piece(Black, King, Square.e8) =>
        adjacent ++ Set(path(2, 0), path(-2, 0))
      case Piece(_, King, _) =>
        adjacent
      case Piece(_, Queen, _) =>
        file ++ rank ++ diagonals
    }

  }

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

  def isValid(move: Move): Boolean = {

    // TODO try to put all castling logic into pathsFor (dont gen path is not allowed)
    def isValidCastle(castle: Castle, piece: Piece) =
      pieceAt(castle.rookMove.start).fold(false)(rookPiece => !hasMoved(piece) && !hasMoved(rookPiece) && paths(rookPiece, pieces - rookPiece).flatten.contains(castle.rookMove.end))

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

    def isBlockingCheck(piece: Piece) = {
      val king = pieces.find(p => p.pieceType == King && p.side == turn).get.square
      pieces.filter(_.side != turn).exists(other => paths(other, pieces - piece).exists(path => path.contains(king) && path.takeWhile(king !=).contains(piece.square)))
    }

    pieceAt(move.start).fold(false) { piece =>
      piece.side == turn &&
        paths(piece, pieces - piece).flatten.contains(move.end) &&
        !isBlockingCheck(piece) &&
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

  def moves: Set[Move] = pieces.flatMap(piece => paths(piece, pieces - piece).flatten.flatMap(Move.moves(piece.square, _).iterator)).filter(isValid)

}
