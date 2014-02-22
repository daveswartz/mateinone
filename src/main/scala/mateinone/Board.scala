package mateinone

import language.postfixOps
import Square._
import File._
import Rank._

object Board {
  def apply(): Board = {
    def piecesForSide(side: Side, pawnRank: Rank, kingRank: Rank): Set[Piece] = {
      def piece(pieceType: PieceType)(square: Square) = Piece(side, pieceType, square, hasMoved = false)
      val pawns = files.map(square(_, pawnRank)).toSet.map(piece(Pawn))
      val rooks = Set(A, H).map(square(_, kingRank)).map(piece(Rook))
      val knights = Set(B, G).map(square(_, kingRank)).map(piece(Knight))
      val bishops = Set(C, F).map(square(_, kingRank)).map(piece(Bishop))
      val king = piece(King)(square(E, kingRank))
      val queen = piece(Queen)(square(D, kingRank))
      pawns ++ rooks ++ knights ++ bishops + king + queen
    }
    Board(White, piecesForSide(White, _2, _1) ++ piecesForSide(Black, _7, _8), None)
  }
}

case class Board private(turn: Side, pieces: Set[Piece], enPassantEnd: Option[File]) {

  private val squareToPiece = pieces.map(p => p.square -> p).toMap
  private def pieceAt(s: Square, f: Piece => Boolean = _ => true) = squareToPiece.get(s).filter(f)
  private def pieceExists(s: Square, f: Piece => Boolean = _ => true) = pieceAt(s, f).isDefined
  private def sameSide(p: Piece) = p.side == turn
  private def otherSide(p: Piece) = p.side != turn
  private val whiteKingside = CastleConcrete(E1, G1, SimpleMove(H1, F1), `O-O`)
  private val whiteQueenside = CastleConcrete(E1, C1, SimpleMove(A1, D1), `O-O-O`)
  private val blackKingside = CastleConcrete(E8, G8, SimpleMove(H8, F8), `O-O`)
  private val blackQueenside = CastleConcrete(E8, C8, SimpleMove(A8, D8), `O-O-O`)
  private object CastleConcrete {
    def apply(c: Castle): CastleConcrete =
      if (turn == White) if (c == `O-O`) whiteKingside else whiteQueenside else if (c == `O-O`) blackKingside else blackQueenside
  }
  private case class CastleConcrete(start: Square, end: Square, rookMove: SimpleMove, castle: Castle)

  val moves: Set[Move] = {

    def paths(piece: Piece, endOnPiece: Boolean = true): Set[List[Square]] = {

      def path(stepOffset: (Int, Int), nSteps: Int = 1, stepOnOther: Boolean = true, stepOnSame: Boolean = false, stepOnEmpty: Boolean = true): List[Square] = {
        def pathRecur(current: List[Square], remaining: Int): List[Square] = {
          if (remaining == 0) current
          else {
            current.last + stepOffset match {
              case None =>
                current
              case Some(next) if pieceExists(next, otherSide) =>
                if (endOnPiece) if (stepOnOther) current :+ next else current else pathRecur(current :+ next, remaining - 1)
              case Some(next) if pieceExists(next, sameSide) =>
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
      def enPassants(start: File, rankStep: Int): Set[List[Square]] =
        enPassantEnd.map(end => Set(1, -1).map(fileStep => if (start + fileStep == Some(end)) path((fileStep, rankStep)) else List())).getOrElse(Set())
      def castles(side: Side, rank: Rank) = {
        def canCastle(side: Side, rook: Square, between: Vector[Square]) = pieceExists(rook, p => !p.hasMoved && sameSide(p)) && between.forall(!pieceExists(_))
        val kingside = canCastle(side, square(H, rank), Vector(F, G).map(square(_, rank)))
        val queenside = canCastle(side, square(A, rank), Vector(B, C, D).map(square(_, rank)))
        Some(path((2, 0))).filter(_ => kingside) ++  Some(path((-2, 0))).filter(_ => queenside)
      }

      piece match {
        case Piece(White, Pawn,   Square(_, `_2`), false) => pawnCaptures(White) + pawnAdvance((0, 1), 2)
        case Piece(White, Pawn,   Square(f, `_5`), true ) => pawnCaptures(White) + pawnAdvance((0, 1), 1) ++ enPassants(f, 1)
        case Piece(White, Pawn,   _,               true ) => pawnCaptures(White) + pawnAdvance((0, 1), 1)
        case Piece(Black, Pawn,   Square(_, `_7`), false) => pawnCaptures(Black) + pawnAdvance((0, -1), 2)
        case Piece(Black, Pawn,   Square(f, `_4`), true ) => pawnCaptures(Black) + pawnAdvance((0, -1), 1) ++ enPassants(f, -1)
        case Piece(Black, Pawn,   _,               true ) => pawnCaptures(Black) + pawnAdvance((0, -1), 1)
        case Piece(_,     Rook,   _,               _    ) => file ++ rank
        case Piece(_,     Knight, _,               _    ) => Set(path((2, 1)), path((2, -1)), path((1, 2)), path((1, -2)), path((-2, 1)), path((-2, -1)), path((-1, 2)), path((-1, -2)))
        case Piece(_,     Bishop, _,               _    ) => diagonals
        case Piece(White, King,   E1,              false) => adjacent ++ castles(White, _1)
        case Piece(Black, King,   E8,              false) => adjacent ++ castles(Black, _8)
        case Piece(_,     King,   _,               _    ) => adjacent
        case Piece(_,     Queen,  _,               _    ) => file ++ rank ++ diagonals
      }

    }

    def moves(start: Square, end: Square, pieceType: PieceType): Set[_ <: Move] = {

      def promotions(starts: List[Square], rankOffset: Int) = for {
        start <- starts
        fileOffset <- List(-1, 0, 1)
        promotionType <- PromotionType.all
        end = start + (fileOffset, rankOffset)
        if end.isDefined
      } yield Promotion(start, end.get, promotionType)
      val allPromotions = promotions(files.toList.map(square(_,_7)), 1) ++ promotions(files.toList.map(square(_,_2)), -1)
      val promotion = new PartialFunction[(Square, Square, PieceType), Set[Promotion]] {
        override def isDefinedAt(args: (Square, Square, PieceType)): Boolean = args._3 == Pawn && allPromotions.exists(p => args._1 == p.start && args._2 == p.end)
        override def apply(args: (Square, Square, PieceType)): Set[Promotion] = PromotionType.all.map(t => Promotion(args._1, args._2, t))
      }

      val allCastles = Set(whiteKingside, whiteQueenside, blackKingside, blackQueenside)
      val castle = new PartialFunction[(Square, Square, PieceType), Set[Castle]] {
        override def isDefinedAt(args: (Square, Square, PieceType)): Boolean = args._3 == King && allCastles.exists(c => args._1 == c.start && args._2 == c.end)
        override def apply(args: (Square, Square, PieceType)): Set[Castle] = allCastles.filter(c => args._1 == c.start && args._2 == c.end).map(_.castle)
      }

      def simple(args: (Square, Square, PieceType)) = Set(SimpleMove(args._1, args._2))

      promotion.orElse(castle).applyOrElse((start, end, pieceType), simple)

    }

    val king = pieces.find(p => sameSide(p) && p.pieceType == King).get.square
    def isBlockingCheck(p: List[Square], s: Square) = p.takeWhile(king !=).contains(s)
    val notBlockingCheck = {
      val potentialChecks = pieces.filter(otherSide).map(paths(_, endOnPiece = false).filter(_.contains(king))).flatten
      pieces.filter(sameSide).filterNot(p => potentialChecks.exists(c => isBlockingCheck(c, p.square))).flatMap(p => paths(p).flatten.map((p.square, _)))
    }
    val checkOption = pieces.filter(otherSide).map(paths(_).filter(_.lastOption == Some(king))).flatten.headOption
    checkOption.fold(notBlockingCheck)(c => notBlockingCheck.filter(t => isBlockingCheck(c, t._2))).flatMap(t => moves(t._1, t._2, pieceAt(t._1).get.pieceType).iterator)

  }

  def isCheckmate = moves.isEmpty

  private def oneMove(nextTurn: Side, start: Square, end: Square, movePiece: Piece => Piece) = {
    val piece = pieceAt(start).get
    val enPassantTarget = piece match {
      case Piece(White, Pawn, Square(f, `_2`), _) if end == square(f, `_4`) => Some(f)
      case Piece(Black, Pawn, Square(f, `_7`), _) if end == square(f, `_5`) => Some(f)
      case _ => None
    }
    Some(Board(nextTurn, pieces.filterNot(_.square == end) - piece + movePiece(piece), enPassantTarget))
  }
  private def moveTo(p: Piece, end: Square): Piece = p.copy(square = end, hasMoved = true)
  private val oneSimpleMove: PartialFunction[Move, Option[Board]] = { case m : SimpleMove if moves.contains(m) => oneMove(turn.other, m.start, m.end, (p: Piece) => moveTo(p, m.end)) }
  private val onePromotion: PartialFunction[Move, Option[Board]] = { case m: Promotion if moves.contains(m) => oneMove(turn.other, m.start, m.end, (p: Piece) => moveTo(p, m.end).copy(pieceType = m.promotionType)) }
  private val oneCastle: PartialFunction[Move, Option[Board]] = { case c: Castle if moves.contains(c) =>
    val m = CastleConcrete(c)
    oneMove(turn, m.start, m.end, (p: Piece) => moveTo(p, m.end)).flatMap(_.oneMove(turn.other, m.rookMove.start, m.rookMove.end, (p: Piece) => moveTo(p, m.rookMove.end)))
  }
  private def oneMove(m : Move): Option[Board] = oneSimpleMove.orElse(onePromotion).orElse(oneCastle).applyOrElse(m, (_: Move) => None)

  def move(moves: Move*): Option[Board] = moves.toList match {
    case head :: tail => oneMove(head).flatMap(_.move(tail :_*))
    case Nil => Some(this)
  }

}
