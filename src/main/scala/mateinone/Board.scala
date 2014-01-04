package mateinone

import Castle._
import OccupiedPath._
import Move._

object Board {

  // Ignores position on the board and whether pieces have moved when generating paths. Castling and two step pawn moves
  // are always generated as paths.
  private def occupiedPathsFor(piece: Piece, otherPieces: Set[Piece]): Set[OccupiedPath] = {

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

    val pathsFor = piece match {
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

    def occupied(path: Path) = otherPieces.map(_.square).filter(path.contains)
    pathsFor.filterNot(_.isEmpty).map(path => OccupiedPath(path, occupied(path)))

  }

  // Checks if the piece is a King and the move is offset by 2 files
  private def mustBeKingsideCastle(piece: Piece, end: Square): Boolean =
    piece.pieceType == King && File.offset(piece.square.file, end.file) == 2

  // Checks if the piece is a King and the move is offset by -2 files
  private def mustBeQueensideCastle(piece: Piece, end: Square): Boolean =
    piece.pieceType == King && File.offset(piece.square.file, end.file) == -2

  private def mustBeCastle(piece: Piece, end: Square): Boolean = mustBeKingsideCastle(piece, end) || mustBeQueensideCastle(piece, end)

  // If a vacant path exists for promotion, the Pawn must be promoted for any valid move
  private def mustBePromotion(piece: Piece): Boolean = {
    piece match {
      case Piece(White, Pawn, Square(_, `7`), _) => true
      case Piece(Black, Pawn, Square(_, `2`), _) => true
      case _ => false
    }
  }

  // Checks if the piece is a Pawn and the move is ofset by two ranks
  private def isTwoSquareAdvance(piece: Piece, end: Square): Boolean =
    piece.pieceType == Pawn && math.abs(Rank.offset(piece.square.rank, end.rank)) == 2

  def apply(): Board = {

    def piece(side: Side, pieceType: PieceType)(square: Square) = Piece(side, pieceType, square, hasMoved = false)

    def piecesFor(side: Side, pawnRank: Rank, kingRank: Rank): Set[Piece] = {
      val pawns = File.allFiles.map(Square(_, pawnRank)).toSet.map(piece(side, Pawn))
      val rooks = Set(A, H).map(Square(_, kingRank)).map(piece(side, Rook))
      val knights = Set(B, G).map(Square(_, kingRank)).map(piece(side, Knight))
      val bishops = Set(C, F).map(Square(_, kingRank)).map(piece(side, Bishop))
      val king = piece(side, King)(Square(E, kingRank))
      val queen = piece(side, Queen)(Square(D, kingRank))
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

  def pieces: Set[Piece] = piecesToOccupiedPaths.keySet

  def pieceAt(square: Square): Option[Piece] = pieces.find(square == _.square)

  def move(moves: Either[Move, Side => Move]*): Option[Board] = {

    def oneMove(move: Move): Option[Board] =
      pieceAt(move.start).filter(_.side == turn).flatMap { piece =>
        piecesToOccupiedPaths.get(piece).flatMap { occupiedPaths =>

          def isValidEnd(end: Square): Boolean = occupiedPaths.find(_.contains(end)).fold(false)(_.isValidEnd(end))

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

          move match {
            case SimpleMove(_, end) =>
              if (isValidEnd(end) && !mustBeCastle(piece, end) && (!isTwoSquareAdvance(piece, end) || !piece.hasMoved) && !mustBePromotion(piece)) {
                doMove(Set(move), Set(piece), Set(piece.atEnd(move)), pieces - piece)
              }
              else None
            case Castle(start, end, rookMove) =>
              pieceAt(rookMove.start).flatMap { rookPiece =>
                if (!piece.hasMoved && !rookPiece.hasMoved && isValidEnd(end) && isValidEnd(rookMove.end))
                  doMove(Set(move, rookMove), Set(piece), Set(piece.atEnd(move), rookPiece.atEnd(rookMove)), pieces - piece - rookPiece)
                else None
              }
            case Promotion(_, end, promotionType) =>
              if (isValidEnd(end) && mustBePromotion(piece))
                doMove(Set(move), Set(piece), Set(piece.atEnd(move).promotedTo(promotionType)), pieces - piece)
              else None
          }

        }
      }

    moves.toList match {
      case head :: tail => oneMove(toMove(head, turn)).flatMap(_.move(tail :_*))
      case Nil => Some(this)
    }

  }

  def moves: Set[Move] =
    piecesToOccupiedPaths.collect {
      case (piece, occupiedPaths) if piece.side == turn =>

        def canCastle(castle: Castle): Boolean = !piece.hasMoved && pieceAt(castle.rookMove.start).fold(false)(!_.hasMoved)

        occupiedPaths.map { occupiedPath =>
          occupiedPath.validEnds.map { end: Square =>
            if (mustBePromotion(piece)) {
              PromotionType.all.map(promotionType => Promotion(piece.square, end, promotionType))
            } else if (mustBeKingsideCastle(piece, end)) {
              val castle = `O-O`(turn)
              if (canCastle(castle)) Set(castle) else Set()
            } else if (mustBeQueensideCastle(piece, end)) {
              val castle = `O-O-O`(turn)
              if (canCastle(castle)) Set(castle) else Set()
            } else if (isTwoSquareAdvance(piece, end) && piece.hasMoved) {
              Set()
            } else {
              Set(SimpleMove(piece.square, end))
            }
          }
        }

    }.toSet.flatten.flatten.flatten

}
