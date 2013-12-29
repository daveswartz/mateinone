package mateinone

import Square._
import Castle._
import OccupiedPath._

object Board {

  private def occupiedPathsFor(piece: Piece, otherPieces: Set[Piece]): Set[OccupiedPath] = {

    def path(square: Square, fileOffset: Int, rankOffset: Int, remaining: Int): Path = {
      def step(fileOffset: Int, rankOffset: Int): Square => Option[Square] = offset(_, fileOffset, rankOffset)
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
        val rankOffset = if (side == White) 1 else -1
        val single = path(square, 0, rankOffset, 1)
        val double = path(square, 0, rankOffset, 2)
        if (!hasMoved) Set(single, double) else Set(single)
      case Piece(_, Rook, square, _) =>
        file(square) ++ rank(square)
      case Piece(_, Knight, square, _) =>
        Set((2, 1), (2, -1), (1, 2), (1, -2), (-2, 1), (-2, -1), (-1, 2), (-1, -2)).map { case (f, r) => path(square, f, r, 1) }
      case Piece(_, Bishop, square, _) =>
        diagonals(square)
      case Piece(side, King, square, hasMoved) =>
        val paths =  Set((0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)).map { case (f, r) => path(square, f, r, 1) }
        if (!hasMoved) {
          def pieceAt(s: Square): Option[Piece] = otherPieces.find(_.square == s)
          val kingside = (if (side == White) pieceAt(g1) else pieceAt(g8)).filter(!_.hasMoved).map(_ => path(square, 2, 0, 1))
          val queenside = (if (side == White) pieceAt(a1) else pieceAt(a8)).filter(!_.hasMoved).map(_ => path(square, -2, 0, 1))
          paths ++ kingside ++ queenside
        } else
          paths
      case Piece(_, Queen, square, _) =>
        file(square) ++ rank(square) ++ diagonals(square)
    }

    def occupied(path: Path) = otherPieces.map(_.square).filter(path.contains)
    pathsFor.filterNot(_.isEmpty).map(path => OccupiedPath(path, occupied(path)))

  }

  private def mustPromote(piece: Piece): Boolean = {
    piece match {
      case Piece(White, Pawn, Square(_, `7`), _) => true
      case Piece(Black, Pawn, Square(_, `2`), _) => true
      case _ => false
    }
  }

  private def isCastleKingside(piece: Piece, end: Square): Boolean = piece match {
    case Piece(White, King, _, false) => end match { case Square(G, `1`) => true case _ => false }
    case Piece(Black, King, _, false) => end match { case Square(G, `8`) => true case _ => false }
    case _ => false
  }

  private def isCastleQueenside(piece: Piece, end: Square): Boolean = piece match {
    case Piece(White, King, _, false) => end match { case Square(C, `1`) => true case _ => false }
    case Piece(Black, King, _, false) => end match { case Square(C, `8`) => true case _ => false }
    case _ => false
  }

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
    }

  }

}
import Board._

trait Board {

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keySet

  def pieceAt(square: Square): Option[Piece] = pieces.find(square == _.square)

  private def oneMove(move: Move): Option[Board] =
    pieceAt(move.start).flatMap { piece =>
      piecesToOccupiedPaths.get(piece).flatMap { occupiedPaths =>

        def isValidEnd(end: Square): Boolean = occupiedPaths.find(_.contains(end)).fold(false)(_.isValidEnd(end))

        def updateOccupiedOf(stationaryPieces: Set[Piece], moves: Set[Move]) =
          stationaryPieces.map(p => (p, piecesToOccupiedPaths(p).map(_.vacate(moves.map(_.start)).occupy(moves.map(_.end))))).toMap

        def updatePathsOf(movedPieces: Set[Piece]) =
          movedPieces.map(p => (p, occupiedPathsFor(p, pieces - p))).toMap

        def doMove(moves: Set[Move], movedPieces: Set[Piece], stationaryPieces: Set[Piece]) =
          Some(new Board { val piecesToOccupiedPaths = updateOccupiedOf(stationaryPieces, moves) ++ updatePathsOf(movedPieces) })

        move match {
          case SimpleMove(_, end) =>
            if (isValidEnd(end) && !mustPromote(piece)) {
              val kingPiece = piece match {
                case Piece(_, Rook, _, false) =>
                  pieceAt(d1) match {
                    case k @ Some(Piece(_, King, _, false)) => k
                    case _ => None
                  }
                case _ => None
              }
              doMove(Set(move), Set(piece.atEnd(move)) ++ kingPiece, pieces - piece)
            }
            else None
          case Castle(start, end, rookMove) =>
            pieceAt(rookMove.start).flatMap { rookPiece =>
              if (!piece.hasMoved && !rookPiece.hasMoved && isValidEnd(end) && isValidEnd(rookMove.end))
                doMove(Set(move, rookMove), Set(piece.atEnd(move), rookPiece.atEnd(rookMove)), pieces - piece - rookPiece)
              else None
            }
          case Promotion(_, end, promotionType) =>
            if (isValidEnd(end) && mustPromote(piece))
              doMove(Set(move), Set(piece.atEnd(move).promotedTo(promotionType)), pieces - piece)
            else None
        }

      }
    }

  def move(moves: Move*): Option[Board] = moves.toList match { // TODO require alternating moves
    case Nil => Some(this)
    case last :: Nil => oneMove(last)
    case head :: tail => oneMove(head).flatMap(_.move(tail :_*))
  }

  def moves: Set[Move] = // TODO require alternating moves
    piecesToOccupiedPaths.map {
      case (piece, occupiedPaths) =>
        occupiedPaths.map { occupiedPath =>
          occupiedPath.validEnds.map { end: Square =>
            if (mustPromote(piece))
              PromotionType.all.map(promotionType => Promotion(piece.square, end, promotionType))
            else if (isCastleKingside(piece, end))
              Set(`O-O`)
            else if (isCastleQueenside(piece, end))
              Set(`O-O-O`)
            else
              Set(SimpleMove(piece.square, end))
          }
        }
    }.toSet.flatten.flatten.flatten

}
