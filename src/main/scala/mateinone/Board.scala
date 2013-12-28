package mateinone

import Square._
import Castle._
import OccupiedPath._

object Board {

  private def occupiedPathsFor(piece: Piece, otherPieces: Set[Piece]): Set[OccupiedPath] = {

    def path(square: Square, step: Square => Option[Square], remaining: Int): Path = {
      def pathRecur(current: Path, step: Square => Option[Square], remaining: Int): Path = step(current.last) match {
        case Some(next) if remaining > 0 => pathRecur(current :+ next, step, remaining - 1)
        case _ => current
      }
      pathRecur(List(square), step, remaining).tail
    }

    def file(square: Square) = Set(
      path(square, Square.offset(_, File.inc, Rank.identity), 7),
      path(square, Square.offset(_, File.dec, Rank.identity), 7)
    )

    def rank(square: Square) = Set(
      path(square, Square.offset(_, File.identity, Rank.inc), 7),
      path(square, Square.offset(_, File.identity, Rank.dec), 7)
    )

    def diagonals(square: Square) = Set(
      path(square, Square.offset(_, File.inc, Rank.inc), 7),
      path(square, Square.offset(_, File.inc, Rank.dec), 7),
      path(square, Square.offset(_, File.dec, Rank.inc), 7),
      path(square, Square.offset(_, File.dec, Rank.dec), 7)
    )

    val pathsForPiece = piece.pieceType match {
      case Pawn =>
        val single = path(piece.square, Square.offset(_, File.identity, Rank.inc), 1) // TODO black is Rank.dec
        if (!piece.hasMoved)
          Set(single, path(piece.square, Square.offset(_, File.identity, Rank.inc), 2)) // TODO black is Rank.dec
        else
          Set(single)
      case Rook =>
        file(piece.square) ++ rank(piece.square)
      case Knight =>
        Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2))
          .flatMap { case (f, r) => Square.offset(piece.square, File.offset(_, f), Rank.offset(_, r)) }.map(List(_))
      case Bishop =>
        diagonals(piece.square)
      case King =>
        var kingPaths = Set(
          path(piece.square, Square.offset(_, File.identity, Rank.inc), 1),
          path(piece.square, Square.offset(_, File.inc, Rank.inc), 1),
          path(piece.square, Square.offset(_, File.inc, Rank.identity), 1),
          path(piece.square, Square.offset(_, File.inc, Rank.dec), 1),
          path(piece.square, Square.offset(_, File.identity, Rank.dec), 1),
          path(piece.square, Square.offset(_, File.dec, Rank.dec), 1),
          path(piece.square, Square.offset(_, File.dec, Rank.identity), 1),
          path(piece.square, Square.offset(_, File.dec, Rank.inc), 1)
        )
        if (!piece.hasMoved) {
          def kingsideRookMoved = otherPieces.find(_.square == g1).fold(false)(!_.hasMoved) // TODO white rook hard-coded
          val kingsideCastle = path(piece.square, Square.offset(_, File.offset(_, 2), Rank.identity), 1)
          if (kingsideRookMoved) kingPaths = kingPaths + kingsideCastle
          def queensideRookMoved = otherPieces.find(_.square == a1).fold(false)(!_.hasMoved) // TODO white rook hard-coded
          val queensideCastle = path(piece.square, Square.offset(_, File.offset(_, -2), Rank.identity), 1)
          if (queensideRookMoved) kingPaths = kingPaths + queensideCastle
        }
        kingPaths
      case Queen =>
        file(piece.square) ++ rank(piece.square) ++ diagonals(piece.square)
    }

    pathsForPiece.filterNot(_.isEmpty).map(p => OccupiedPath(p, otherPieces.map(_.square).filter(p.contains)))

  }

  def apply(): Board = {
    def piece(side: Side, pieceType: PieceType)(square: Square) = Piece(side, pieceType, square, hasMoved = false)

    val whitePieces = {
      val pawns = File.allFiles.map(Square(_, `2`)).toSet.map(piece(White, Pawn))
      val rooks = Set(A, H).map(Square(_, `1`)).map(piece(White, Rook))
      val knights = Set(B, G).map(Square(_, `1`)).map(piece(White, Knight))
      val bishops = Set(C, F).map(Square(_, `1`)).map(piece(White, Bishop))
      val queen = piece(White, Queen)(d1)
      val king = piece(White, King)(e1)

      pawns ++ rooks ++ knights ++ bishops + king + queen
    } // TODO only white now

    val initialPiecesToOccupiedPaths = whitePieces.map(piece => (piece, occupiedPathsFor(piece, whitePieces - piece))).toMap

    new Board { val piecesToOccupiedPaths = initialPiecesToOccupiedPaths }
  }

}
import Board._

trait Board {

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keySet

  def pieceAt(square: Square): Option[Piece] = pieces.find(square == _.square)

  private def mustPromote(piece: Piece, end: Square): Boolean = piece.pieceType == Pawn && end.rank == `8` // TODO support black

  private def mayCastleKingside(piece: Piece, end: Square): Boolean = piece.pieceType == King && end == g1 // TODO support black

  private def mayCastleQueenside(piece: Piece, end: Square): Boolean = piece.pieceType == King && end == c1 // TODO support black

  private def oneMove(move: Move): Option[Board] =
    pieceAt(move.start).flatMap { piece =>
      piecesToOccupiedPaths.get(piece).flatMap { occupiedPaths =>

        def isValidEnd(end: Square): Boolean = occupiedPaths.find(_.contains(end)).fold(false)(_.isValidEnd(end))

        def updateOccupiedPathsFor(moves: Set[Move], stationaryPieces: Set[Piece]) =
          stationaryPieces.map(p => (p, piecesToOccupiedPaths(p).map(_.vacate(moves.map(_.start)).occupy(moves.map(_.end))))).toMap

        def replaceOccupiedPathsFor(movedPieces: Set[Piece]) =
          movedPieces.map(p => (p, occupiedPathsFor(p, pieces - p))).toMap

        def doMove(moves: Set[Move], movedPieces: Set[Piece], stationaryPieces: Set[Piece]) = {
          val stationaryEntries = updateOccupiedPathsFor(moves, stationaryPieces)
          val movedEntries = replaceOccupiedPathsFor(movedPieces)
          Some(new Board { val piecesToOccupiedPaths = stationaryEntries ++ movedEntries })
        }

        move match {
          case SimpleMove(_, end) =>
            if (isValidEnd(end) && !mustPromote(piece, end))
              doMove(Set(move), Set(piece.atEnd(move)), pieces - piece)
            else None
          case Castle(start, end, rookMove) =>
            pieceAt(rookMove.start).flatMap { rookPiece =>
              if (isValidEnd(end) && isValidEnd(rookMove.end))
                doMove(Set(move, rookMove), Set(piece.atEnd(move), rookPiece.atEnd(rookMove)), pieces - piece - rookPiece)
              else None
            }
          case Promotion(_, end, promotionType) =>
            if (isValidEnd(end) && mustPromote(piece, end))
              doMove(Set(move), Set(piece.atEnd(move).promotedTo(promotionType)), pieces - piece)
            else None
        }

      }
    }

  def move(moves: Move*): Option[Board] = moves.toList match {
    case Nil => Some(this)
    case last :: Nil => oneMove(last)
    case head :: tail => oneMove(head).flatMap(_.move(tail :_*))
  }

  def moves: Set[Move] = // TODO support alternating moves
    piecesToOccupiedPaths.map {
      case (piece, occupiedPaths) =>
        occupiedPaths.map { occupiedPath =>
          occupiedPath.validEnds.map { end: Square =>
            if (mustPromote(piece, end))
              PromotionType.all.map(promotionType => Promotion(piece.square, end, promotionType))
            else if (mayCastleKingside(piece, end))
              Set(`O-O`)
            else if (mayCastleQueenside(piece, end))
              Set(`O-O-O`)
            else
              Set(SimpleMove(piece.square, end))
          }
        }
    }.toSet.flatten.flatten.flatten

}
