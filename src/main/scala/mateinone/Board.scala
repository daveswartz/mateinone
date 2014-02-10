package mateinone

import language.postfixOps

object Board {
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

case class Board private(turn: Side, pieces: Set[Piece], lastMove: Option[Move] = None) {
  import Piece._
  import Square._

  val moves: Set[Move] = {

    val sameSide = withSide(pieces, turn)
    val otherSide = withSide(pieces, turn.other)

    def paths(piece: Piece, endOnPiece: Boolean = true): Set[List[Square]] = {

      def path(stepOffset: (Int, Int), nSteps: Int = 1, stepOnOther: Boolean = true, stepOnSame: Boolean = false, stepOnEmpty: Boolean = true): List[Square] = {
        def pathRecur(current: List[Square], remaining: Int): List[Square] = {
          def hasSame(s: Square) = withSquare(sameSide, s).size > 0
          def hasOther(s: Square) = withSquare(otherSide, s).size > 0
          if (remaining == 0) current
          else {
            current.last.offset(stepOffset) match {
              case None =>
                current
              case Some(next) if hasOther(next) =>
                if (endOnPiece) if (stepOnOther) current :+ next else current else pathRecur(current :+ next, remaining - 1)
              case Some(next) if hasSame(next) =>
                if (endOnPiece) if (stepOnSame) current :+ next else current else pathRecur(current :+ next, remaining - 1)
              case Some(next) =>
                if (stepOnEmpty) pathRecur(current :+ next, remaining - 1) else current
            }
          }
        }
        pathRecur(List(piece.square), nSteps).tail
      }

      def file = Set(path((1, 0), 7), path((-1, 0), 7))
      def rank = Set(path((0, 1), 7), path((0, -1), 7))
      def diagonals = Set(path((1, 1), 7), path((1, -1), 7), path((-1, 1), 7), path((-1, -1), 7))
      def adjacent = Set(path((0, 1)), path((1, 1)), path((1, 0)), path((1, -1)), path((0, -1)), path((-1, -1)), path((-1, 0)), path((-1, 1)))
      def pawnAdvance = path(_: (Int, Int), _: Int, stepOnOther = false)
      def pawnCaptures(side: Side) = {
        val one = path(_: (Int, Int), 1, stepOnEmpty = false)
        if (side == White) Set(one(1, 1), one(-1, 1)) else Set(one(1, -1), one(-1, -1))
      }
      def enPassants(side: Side) = {
        def one(stepOffset: (Int, Int)) = {
          val target: Option[Square] = piece.square.offset(stepOffset._1, 0)
          new PartialFunction[Option[Move], List[Square]] {
            def isDefinedAt(x: Option[Move]): Boolean = x.fold(false)(m => target == Some(m.end) && Set((0, -2), (0, 2)).contains(m.offset))
            def apply(x: Option[Move]): List[Square] = path(stepOffset)
          }.applyOrElse(lastMove, (_: Option[Move]) => List())
        }
        if (side == White) Set(one(1, 1), one(-1, 1)) else Set(one(1, -1), one(-1, -1))
      }
      def canCastle(side: Side, rook: Square, between: Set[Square]): Boolean =
        thatHaveNotMoved(withSquare(sameSide, rook)).size > 0 && between.forall(b => withSquare(sameSide, b).isEmpty)
      def castleWhiteKingside = if (canCastle(White, h1, Set(f1, g1))) path((2, 0)) else List()
      def castleWhiteQueenside = if (canCastle(White, a1, Set(b1, c1, d1))) path((-2, 0)) else List()
      def castleBlackKingside = if (canCastle(Black, h8, Set(f8, g8))) path((2, 0)) else List()
      def castleBlackQueenside = if (canCastle(Black, a8, Set(b8, c8, d8))) path((-2, 0)) else List()

      piece match {
        case Piece(White, Pawn,   Square(_, `2`), false) => pawnCaptures(White) + pawnAdvance((0, 1), 2)
        case Piece(White, Pawn,   Square(_, `5`), true ) => pawnCaptures(White) + pawnAdvance((0, 1), 1) ++ enPassants(White)
        case Piece(White, Pawn,   _,              true ) => pawnCaptures(White) + pawnAdvance((0, 1), 1)
        case Piece(Black, Pawn,   Square(_, `7`), false) => pawnCaptures(Black) + pawnAdvance((0, -1), 2)
        case Piece(Black, Pawn,   Square(_, `4`), true ) => pawnCaptures(Black) + pawnAdvance((0, -1), 1) ++ enPassants(Black)
        case Piece(Black, Pawn,   _,              true ) => pawnCaptures(Black) + pawnAdvance((0, -1), 1)
        case Piece(_,     Rook,   _,              _    ) => file ++ rank
        case Piece(_,     Knight, _,              _    ) => Set(path((2, 1)), path((2, -1)), path((1, 2)), path((1, -2)), path((-2, 1)), path((-2, -1)), path((-1, 2)), path((-1, -2)))
        case Piece(_,     Bishop, _,              _    ) => diagonals
        case Piece(White, King,   Square(E, `1`), false) => adjacent + castleWhiteKingside + castleWhiteQueenside
        case Piece(Black, King,   Square(E, `8`), false) => adjacent + castleBlackKingside + castleBlackQueenside
        case Piece(_,     King,   _,              _    ) => adjacent
        case Piece(_,     Queen,  _,              _    ) => file ++ rank ++ diagonals
      }

    }

    val king = withPieceType(sameSide, King).head.square
    val potentialChecks = otherSide.map(paths(_, endOnPiece = false).filter(_.contains(king))).flatten
    def isBlockingCheck(p: List[Square], s: Square) = p.takeWhile(king !=).contains(s)
    val notBlockingCheck = sameSide.filterNot(s => potentialChecks.exists(c => isBlockingCheck(c, s.square))).flatMap(s => paths(s).flatten.map((s.square, _)))
    val checkOption = otherSide.map(paths(_).filter(_.lastOption == Some(king))).flatten.headOption
    checkOption.fold(notBlockingCheck)(c => notBlockingCheck.filter(t => isBlockingCheck(c, t._2))).flatMap(t => Move.moves(t._1, t._2).iterator)

  }

  // Returns `Some[Board]` when the moves are valid; otherwise, `None`. The repeated parameter is either a `Move`
  // instance or a function that takes a `Side` and returns a `Move`. The reason for using an `Either` is to allow
  // castling to be specified as `O-O` or `O-O-O` without requiring the side to be specified.
  def move(movesToMake: Either[Move, Side => Move]*): Option[Board] = {

    def oneMove(move: Move): Option[Board] =
      if (moves.contains(move)) {
        val piece = withSquare(pieces, move.start).head
        val nextPieces = move match {
          case SimpleMove(_, end) =>
            pieces.filterNot(_.square == end) - piece + piece.movedTo(end)
          case Castle(_, end, SimpleMove(rookStart, rookEnd)) =>
            val rookPiece = withSquare(pieces, rookStart).head
            pieces - piece - rookPiece + piece.movedTo(end) + rookPiece.movedTo(rookEnd)
          case Promotion(_, end, promotionType) =>
            pieces.filterNot(_.square == end) - piece + piece.movedTo(end).promotedTo(promotionType)
        }
        Some(new Board(turn.other, nextPieces, Some(move)))
      } else None

    movesToMake.toList match {
      case head :: tail => oneMove(Move.toMove(head, turn)).flatMap(_.move(tail :_*))
      case Nil => Some(this)
    }

  }

}
