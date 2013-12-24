package mateinone

import Square._

sealed trait PieceType

object PromotionType { val all = Set(Knight, Rook, Bishop, Queen) }
sealed trait PromotionType extends PieceType

case object Pawn extends PieceType
case object King extends PieceType
case object Knight extends PromotionType
case object Rook extends PromotionType
case object Bishop extends PromotionType
case object Queen extends PromotionType

object Piece {
  val pawn = Piece(Pawn, _: Square, _: Boolean)
  val king = Piece(King, _: Square, _: Boolean)
  val knight = Piece(Knight, _: Square, _: Boolean)
  val rook = Piece(Rook, _: Square, _: Boolean)
  val bishop = Piece(Bishop, _: Square, _: Boolean)
  val queen = Piece(Queen, _: Square, _: Boolean)
  def unmoved(pieceOf: (Square, Boolean) => Piece) = pieceOf(_: Square, false)
  def moved(pieceOf: (Square, Boolean) => Piece) = pieceOf(_: Square, true)
}
case class Piece(pieceType: PieceType, square: Square, hasMoved: Boolean) {
  def atEnd(end: Square): Piece = copy(square = end, hasMoved = true)
}

sealed trait Move { val start: Square; val end: Square }

object SimpleMove {
  implicit def tupleToSimpleMove(t: (Square, Square)): SimpleMove = t match { case (start, end) => SimpleMove(start, end) }
}
case class SimpleMove(start: Square, end: Square) extends Move

case class Promotion(start: Square, end: Square, promotionType: PromotionType) extends Move

object Castle {
  val `O-O` = Castle(e1, g1, SimpleMove(h1, f1))
  val `O-O-O` = Castle(e1, c1, SimpleMove(a1, d1))
}
sealed case class Castle(start: Square, end: Square, rookMove: SimpleMove) extends Move

object Board {

  private type Path = List[Square]

  case class OccupiedPath(private val path: Path, private val occupied: Set[Square]) {
    def contains(s: Square): Boolean = path.contains(s)
    def vacate(s: Set[Square]): OccupiedPath = copy(occupied = this.occupied -- s)
    def occupy(s: Set[Square]): OccupiedPath = copy(occupied = this.occupied ++ s.filter(contains))
    def validEnds: Set[Square] = {
      def firstOccupiedInx = occupied.map(s => path.indexOf(s)).min
      if (occupied.isEmpty) path.toSet else path.splitAt(firstOccupiedInx)._1.toSet
    }
    def isValidEnd(end: Square): Boolean = validEnds.contains(end)
  }

  private def occupiedPathsFor(piece: Piece, pieces: Set[Piece]): Set[OccupiedPath] = {

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
        val single = path(piece.square, Square.offset(_, File.identity, Rank.inc), 1)
        if (!piece.hasMoved)
          Set(single, path(piece.square, Square.offset(_, File.identity, Rank.inc), 2))
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
          def kingsideRookMoved = pieces.find(_.square == g1).fold(false)(!_.hasMoved)
          val kingsideCastle = path(piece.square, Square.offset(_, File.offset(_, 2), Rank.identity), 1)
          if (kingsideRookMoved) kingPaths = kingPaths + kingsideCastle
          def queensideRookMoved = pieces.find(_.square == a1).fold(false)(!_.hasMoved)
          val queensideCastle = path(piece.square, Square.offset(_, File.offset(_, -2), Rank.identity), 1)
          if (queensideRookMoved) kingPaths = kingPaths + queensideCastle
        }
        kingPaths
      case Queen =>
        file(piece.square) ++ rank(piece.square) ++ diagonals(piece.square)
    }

    pathsForPiece.filterNot(_.isEmpty).map(p => OccupiedPath(p, pieces.map(_.square).filter(p.contains)))

  }

  def apply(): Board = {
    def unmoved(pieceOf: (Square, Boolean) => Piece) = pieceOf(_: Square, false)

    val pawns = File.allFiles.map(Square(_, `2`)).toSet.map(unmoved(Piece.pawn))
    val rooks = Set(A, H).map(Square(_, `1`)).map(unmoved(Piece.rook))
    val knights = Set(B, G).map(Square(_, `1`)).map(unmoved(Piece.knight))
    val bishops = Set(C, F).map(Square(_, `1`)).map(unmoved(Piece.bishop))
    val queen = Set(d1).map(unmoved(Piece.queen))
    val king = Set(e1).map(unmoved(Piece.king))

    val pieces = pawns ++ rooks ++ knights ++ bishops ++ king ++ queen
    val initialPiecesToOccupiedPaths = pieces.map(piece => (piece, occupiedPathsFor(piece, pieces))).toMap

    new Board { val piecesToOccupiedPaths = initialPiecesToOccupiedPaths }
  }

}

trait Board {
  import Board._

  protected val piecesToOccupiedPaths: Map[Piece, Set[OccupiedPath]]

  def pieces: Set[Piece] = piecesToOccupiedPaths.keySet

  def pieceAt(square: Square): Option[Piece] = pieces.find(square == _.square)

  private def canPromote(piece: Piece, end: Square): Boolean = piece.pieceType == Pawn && end.rank == `8`

  private def oneMove(move: Move): Option[Board] =
    pieceAt(move.start).flatMap { piece =>
      piecesToOccupiedPaths.get(piece).flatMap { occupiedPaths =>

        def isValidEnd(end: Square): Boolean = occupiedPaths.find(_.contains(end)).fold(false)(_.isValidEnd(end))

        def updateOccupiedPathsFor(somePieces: Set[Piece], moves: Set[Move]) =
          somePieces.map { piece => (piece, piecesToOccupiedPaths(piece).map(_.vacate(moves.map(_.start)).occupy(moves.map(_.end)))) }.toMap

        def replaceOccupiedPathsFor(pieceAndMoves: Set[(Piece, Move)]) =
          pieceAndMoves.map { case (p, m) => val pe = p.atEnd(m.end); (pe, occupiedPathsFor(pe, pieces)) }.toMap

        def doMove(piecesAndMoves: Set[(Piece, Move)]) = {
          val (p, m) = piecesAndMoves.unzip
          val unmoved = updateOccupiedPathsFor(pieces -- p, m)
          val moved = replaceOccupiedPathsFor(piecesAndMoves)
          Some(new Board { val piecesToOccupiedPaths = unmoved ++ moved })
        }

        move match {
          case SimpleMove(_, end) =>
            if (isValidEnd(end)) doMove(Set((piece, move))) else None
          case Castle(start, end, rookMove) =>
            pieceAt(rookMove.start).flatMap { rookPiece =>
              if (isValidEnd(end) && isValidEnd(rookMove.end)) doMove(Set((piece, move), (rookPiece, rookMove))) else None
            }
          case Promotion(_, end, promotion) =>
            if (isValidEnd(end) && canPromote(piece, end)) doMove(Set((piece, move))) else None
        }

      }
    }

  def move(moves: Move*): Option[Board] = moves.toList match {
    case Nil => Some(this)
    case last :: Nil => oneMove(last)
    case head :: tail => oneMove(head).flatMap(_.move(tail :_*))
  }

  def moves: Set[Move] =
    piecesToOccupiedPaths.map {
      case (piece, occupiedPaths) =>
        occupiedPaths.map { occupiedPath =>
          occupiedPath.validEnds.map { end: Square =>
            if (canPromote(piece, end))
              PromotionType.all.map(promotionType => Promotion(piece.square, end, promotionType))
            else
              Set(SimpleMove(piece.square, end))
          }
        }
    }.toSet.flatten.flatten.flatten

}
