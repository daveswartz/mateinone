package mateinone

import OccupiedPath._
import Move._

object Board {

  // Generates an indexed sequence of squares for the specified piece corresponding to moves the piece can make if it is
  // the only piece on the board in any square. I.e., castling, two square pawn advance, pawn capture and en passant
  // are always generated.
  private def pathsFor(piece: Piece): Set[Path] = {

    def path(square: Square, fileOffset: Int, rankOffset: Int, remaining: Int): Path = {
      def step(fileOffset: Int, rankOffset: Int): Square => Option[Square] = Square.offset(_, fileOffset, rankOffset)
      def pathRecur(current: Path, step: Square => Option[Square], remaining: Int): Path = step(current.last) match {
        case Some(next) if remaining > 0 => pathRecur(current :+ next, step, remaining - 1)
        case _ => current
      }
      pathRecur(List(square), step(fileOffset, rankOffset), remaining).tail
    }

    def file(s: Square) = Set(path(s, 1, 0, 7), path(s, -1, 0, 7))
    def rank(s: Square) = Set(path(s, 0, 1, 7), path(s, 0, -1, 7))
    def diagonals(s: Square) = Set(path(s, 1, 1, 7), path(s, 1, -1, 7), path(s, -1, 1, 7), path(s, 1, -1, 7))

    val paths = piece match {
      case Piece(side, Pawn, square, hasMoved) =>
        val dir = if (side == White) 1 else -1
        Set(path(square, 0, dir, 2))
      case Piece(_, Rook, square, _) =>
        file(square) ++ rank(square)
      case Piece(_, Knight, square, _) =>
        Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2)).map { case (f, r) => path(square, f, r, 1) }
      case Piece(_, Bishop, square, _) =>
        diagonals(square)
      case Piece(_, King, square, _) =>
        Set((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (2, 0), (-2, 0)).map { case (f, r) => path(square, f, r, 1) }
      case Piece(_, Queen, square, _) =>
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

    def piece(side: Side, pieceType: PieceType)(square: Square) = Piece(side, pieceType, square, hasMoved = false)

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
      val piecesToOccupiedPaths = initialPieces.map(piece => (piece, occupiedPathsFor(piece, initialPieces - piece))).toMap
      val turn = White
    }

  }

}
import Board._

trait Board {

  val turn: Side

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  private def endsFor(piece: Piece): Set[Square] = piecesToOccupiedPaths(piece).flatMap(_.validEnds)

  def pieces: Set[Piece] = piecesToOccupiedPaths.keySet

  def pieceAt(square: Square): Option[Piece] = pieces.find(_.square == square)

  // Returns true when the move is valid; otherwise, false.
  def isValid(move: Move): Boolean = {

    def isValidCastle(castle: Castle, piece: Piece) =
      pieceAt(castle.rookMove.start).fold(false)(rookPiece => !piece.hasMoved && !rookPiece.hasMoved && endsFor(rookPiece).contains(castle.rookMove.end))

    def isValidSimpleMove(simpleMove: SimpleMove, piece: Piece) = {

      def mustBeCastle(piece: Piece, end: Square) =
        piece.pieceType == King && math.abs(File.offset(piece.square.file, end.file)) == 2

      def mustBePromotion(piece: Piece) =
        piece.pieceType == Pawn && (piece.side == White && piece.square.rank == `7`) || (piece.side == Black && piece.square.rank == `2`)

      def isInvalidTwoSquareAdvance(piece: Piece, end: Square) =
        piece.pieceType == Pawn && math.abs(Rank.offset(piece.square.rank, end.rank)) == 2 && piece.hasMoved

      !mustBePromotion(piece) && !mustBeCastle(piece, simpleMove.end) && !isInvalidTwoSquareAdvance(piece, simpleMove.end)

    }

    pieceAt(move.start).fold(false) { piece =>
      piece.side == turn &&
        endsFor(piece).contains(move.end) &&
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
        Some(new Board {
          val piecesToOccupiedPaths = updateOccupiedOf(stationary, movesMade) ++ updatePathsOf(movedBefore, movedAfter)
          val turn = nextTurn
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
      val promotions: Set[Move] = PromotionType.all.flatMap(promotionType => Promotion.optionally(piece.square, end, promotionType))
      val castles: Set[Move] = Castle.all.map(_.rookMove).flatMap(Castle.optionally(piece.square, end, _))
      val simple: Move = SimpleMove(piece.square, end)
      promotions ++ castles + simple
    }

    piecesToOccupiedPaths.flatMap {
      case (piece, occupiedPaths) =>
        occupiedPaths.flatMap { occupiedPath =>
          occupiedPath.validEnds.flatMap { end: Square =>
            movesFor(piece, end).filter(isValid)
          }
        }
    }.toSet

  }

}
