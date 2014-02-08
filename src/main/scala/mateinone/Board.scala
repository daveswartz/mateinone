package mateinone

import Move._

object Board {

  private def paths(piece: Piece, others: Set[Piece], lastOption: Option[Move]): Set[List[Square]] = {

    def path(stepOffset: (Int, Int), nSteps: Int = 1, stepOnCapture: Boolean = true, stepOnEmpty: Boolean = true): List[Square] = {
      def pathRecur(current: List[Square], remaining: Int): List[Square] = {
        val nextOption = current.last.offset(stepOffset)
        def findOther(side: Side, square: Square): Option[Piece] = others.find(o => o.side == side && o.square == square)
        def isCapture(next: Square): Boolean = findOther(piece.side.other, next).isDefined
        nextOption match {
          case Some(next) if findOther(piece.side, next).isDefined => current
          case Some(next) if stepOnCapture && isCapture(next) => current :+ next
          case Some(next) if !isCapture(next) && stepOnEmpty && remaining > 0 => pathRecur(current :+ next, remaining - 1)
          case _ => current
        }
      }
      pathRecur(List(piece.square), nSteps).tail
    }

    def file = Set(path((1, 0), 7), path((-1, 0), 7))
    def rank = Set(path((0, 1), 7), path((0, -1), 7))
    def diagonals = Set(path((1, 1), 7), path((1, -1), 7), path((-1, 1), 7), path((-1, -1), 7))
    def adjacent = Set(path((0, 1)), path((1, 1)), path((1, 0)), path((1, -1)), path((0, -1)), path((-1, -1)), path((-1, 0)), path((-1, 1)))
    val pawnAdvance = path(_: (Int, Int), _: Int, stepOnCapture = false)
    val pawnCapture = path(_: (Int, Int), 1, stepOnEmpty = false)
    def enPassant(stepOffset: (Int, Int)) = lastOption match {
      case Some(SimpleMove(start, end)) if piece.square.offset(stepOffset._1, 0) == Some(end) && Square.offset(start, end) == (if (piece.side == White) (0, -2) else (0, 2)) =>
        path(stepOffset)
      case _ =>
        List()
    }

    piece match {
      case Piece(White, Pawn, Square(_, `2`), false) =>
        Set(pawnAdvance((0, 1), 2), pawnCapture(1, 1), pawnCapture(-1, 1))
      case Piece(White, Pawn, _, _) =>
        Set(pawnAdvance((0, 1), 1), pawnCapture(1, 1), pawnCapture(-1, 1), enPassant((1, 1)), enPassant((-1, 1)))
      case Piece(Black, Pawn, Square(_, `7`), false) =>
        Set(pawnAdvance((0, -1), 2), pawnCapture(1, -1), pawnCapture(-1, -1))
      case Piece(Black, Pawn, _, _) =>
        Set(pawnAdvance((0, -1), 1), pawnCapture(1, -1), pawnCapture(-1, -1), enPassant((1, -1)), enPassant((-1, -1)))
      case Piece(_, Rook, _, _) =>
        file ++ rank
      case Piece(_, Knight, _, _) =>
        Set(path((2, 1)), path((2, -1)), path((1, 2)), path((1, -2)), path((-2, 1)), path((-2, -1)), path((-1, 2)), path((-1, -2)))
      case Piece(_, Bishop, _, _) =>
        diagonals
      case Piece(White, King, Square.e1, false) =>
        adjacent ++ Set(path((2, 0)), path((-2, 0)))
      case Piece(Black, King, Square.e8, false) =>
        adjacent ++ Set(path((2, 0)), path((-2, 0)))
      case Piece(_, King, _, _) =>
        adjacent
      case Piece(_, Queen, _, _) =>
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
    Board(White, piecesFor(White, `2`, `1`) ++ piecesFor(Black, `7`, `8`))
  }

}
import Board._

case class Board private(turn: Side, pieces: Set[Piece], lastMove: Option[Move] = None) {

  def pieceAt(square: Square): Option[Piece] = pieces.find(_.square == square)

  // Returns `Some[Board]` when the moves are valid; otherwise, `None`. The repeated parameter is either a `Move`
  // instance or a function that takes a `Side` and returns a `Move`. The reason for using an `Either` is to allow
  // castling to be specified as `O-O` or `O-O-O` without requiring the side to be specified.
  def move(movesToMake: Either[Move, Side => Move]*): Option[Board] = {

    def oneMove(move: Move): Option[Board] =
      if (moves.contains(move)) {
        val piece = pieceAt(move.start).get
        val nextPieces = move match {
          case SimpleMove(_, end) =>
            pieces.filterNot(_.square == end) - piece + piece.movedTo(end)
          case Castle(_, end, SimpleMove(rookStart, rookEnd)) =>
            val rookPiece = pieceAt(rookStart).get
            pieces - piece - rookPiece + piece.movedTo(end) + rookPiece.movedTo(rookEnd)
          case Promotion(_, end, promotionType) =>
            pieces.filterNot(_.square == end) - piece + piece.movedTo(end).promotedTo(promotionType)
        }
        Some(new Board(turn.other, nextPieces, Some(move)))
      } else None

    movesToMake.toList match {
      case head :: tail => oneMove(toMove(head, turn)).flatMap(_.move(tail :_*))
      case Nil => Some(this)
    }

  }

  def moves: Set[Move] = {
    def isValid(move: Move): Boolean = {

      def isValidCastle(castle: Castle, piece: Piece) =
        pieceAt(castle.rookMove.start).fold(false)(rookPiece => !rookPiece.hasMoved && paths(rookPiece, pieces - rookPiece, lastMove).flatten.contains(castle.rookMove.end))

      def isBlockingCheck(piece: Piece) = {
        val king = pieces.find(p => p.pieceType == King && p.side == turn).get.square
        pieces.filter(_.side != turn).exists(other => paths(other, pieces - piece, lastMove).exists(path => path.contains(king) && path.takeWhile(king !=).contains(piece.square)))
      }

      pieceAt(move.start).fold(false) { piece =>
        piece.side == turn &&
          paths(piece, pieces - piece, lastMove).flatten.contains(move.end) &&
          !isBlockingCheck(piece) &&
          (move match {
            case castle: Castle => isValidCastle(castle, piece)
            case promotion: Promotion => true
            case simpleMove: SimpleMove => true
          })
      }

    }

    pieces.flatMap(piece => paths(piece, pieces - piece, lastMove).flatten.flatMap(Move.moves(piece.square, _).iterator)).filter(isValid)
  }

}
