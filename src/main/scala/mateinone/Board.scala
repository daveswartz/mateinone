package mateinone

import language.postfixOps
import Square._
import File._
import Rank._

object Board {
  def apply(): Board = {
    def piecesForSide(side: Side, pawnRank: Rank, kingRank: Rank): Vector[Piece] = {
      def piece(pieceType: PieceType)(square: Square) = Piece(side, pieceType, square, hasMoved = false)
      val pawns = files.map(square(_, pawnRank)).map(piece(Pawn))
      val rooks = Vector(A, H).map(square(_, kingRank)).map(piece(Rook))
      val knights = Vector(B, G).map(square(_, kingRank)).map(piece(Knight))
      val bishops = Vector(C, F).map(square(_, kingRank)).map(piece(Bishop))
      val king = piece(King)(square(E, kingRank))
      val queen = piece(Queen)(square(D, kingRank))
      pawns ++ rooks ++ knights ++ bishops :+ king :+ queen
    }
    Board(White, piecesForSide(White, _2, _1) ++ piecesForSide(Black, _7, _8), None)
  }
}

case class Board private(turn: Side, pieces: Vector[Piece], enPassantEnd: Option[File]) {

  private val squareToPiece = pieces.map(p => p.square -> p).toMap
  private def pieceAt(s: Square, f: Piece => Boolean = _ => true) = squareToPiece.get(s).filter(f)
  private def pieceExists(s: Square, f: Piece => Boolean = _ => true) = pieceAt(s, f).isDefined
  private val whiteKingside = CastleConcrete(E1, G1, SimpleMove(H1, F1), `O-O`)
  private val whiteQueenside = CastleConcrete(E1, C1, SimpleMove(A1, D1), `O-O-O`)
  private val blackKingside = CastleConcrete(E8, G8, SimpleMove(H8, F8), `O-O`)
  private val blackQueenside = CastleConcrete(E8, C8, SimpleMove(A8, D8), `O-O-O`)
  private object CastleConcrete {
    def apply(c: Castle): CastleConcrete =
      if (turn == White) if (c == `O-O`) whiteKingside else whiteQueenside else if (c == `O-O`) blackKingside else blackQueenside
  }
  private case class CastleConcrete(start: Square, end: Square, rookMove: SimpleMove, castle: Castle)

  val moves: Vector[Move] = {

    def paths(piece: Piece, endOnPiece: Boolean = true): Vector[Vector[Square]] = {

      def path(stepOffset: (Int, Int), nSteps: Int = 1, stepOnOther: Boolean = true, stepOnSame: Boolean = false, stepOnEmpty: Boolean = true): Vector[Square] = {
        def pathRecur(current: Vector[Square], remaining: Int): Vector[Square] = {
          if (remaining == 0) current
          else {
            current.last + stepOffset match {
              case None =>
                current
              case Some(next) if pieceExists(next, _.side != piece.side) =>
                if (endOnPiece) if (stepOnOther) current :+ next else current else pathRecur(current :+ next, remaining - 1)
              case Some(next) if pieceExists(next, _.side == piece.side) =>
                if (endOnPiece) if (stepOnSame) current :+ next else current else pathRecur(current :+ next, remaining - 1)
              case Some(next) =>
                if (stepOnEmpty) pathRecur(current :+ next, remaining - 1) else current
            }
          }
        }
        pathRecur(Vector(piece.square), nSteps).tail
      }

      def file = Vector(path((1, 0), 7), path((-1, 0), 7))
      def rank = Vector(path((0, 1), 7), path((0, -1), 7))
      def diagonals = Vector(path((1, 1), 7), path((1, -1), 7), path((-1, 1), 7), path((-1, -1), 7))
      def adjacent = Vector(path((0, 1)), path((1, 1)), path((1, 0)), path((1, -1)), path((0, -1)), path((-1, -1)), path((-1, 0)), path((-1, 1)))
      def pawnAdvance(s: (Int, Int), n: Int) = path(s, n, stepOnOther = false)
      def pawnCaptures(side: Side) = {
        val one = path(_: (Int, Int), 1, stepOnEmpty = false)
        if (side == White) Vector(one(1, 1), one(-1, 1)) else Vector(one(1, -1), one(-1, -1))
      }
      def enPassants(start: File, rankStep: Int): Vector[Vector[Square]] =
        enPassantEnd.map(end => Vector(1, -1).map(fileStep => if (start + fileStep == Some(end)) path((fileStep, rankStep)) else Vector())).getOrElse(Vector())
      def castles(side: Side, rank: Rank): Vector[Vector[Square]] = {
        def canCastle(side: Side, rook: Square, between: Vector[Square]) = pieceExists(rook, p => !p.hasMoved && p.side == turn) && between.forall(!pieceExists(_))
        val kingside = canCastle(side, square(H, rank), Vector(F, G).map(square(_, rank)))
        val queenside = canCastle(side, square(A, rank), Vector(B, C, D).map(square(_, rank)))
        Vector(path((2, 0))).filter(_ => kingside) ++ Vector(path((-2, 0))).filter(_ => queenside)
      }

      val all = piece match {
        case Piece(White, Pawn,   Square(_, `_2`), false) => pawnCaptures(White) :+ pawnAdvance((0, 1), 2)
        case Piece(White, Pawn,   Square(f, `_5`), true ) => pawnCaptures(White) ++ enPassants(f, 1) :+ pawnAdvance((0, 1), 1)
        case Piece(White, Pawn,   _,               true ) => pawnCaptures(White) :+ pawnAdvance((0, 1), 1)
        case Piece(Black, Pawn,   Square(_, `_7`), false) => pawnCaptures(Black) :+ pawnAdvance((0, -1), 2)
        case Piece(Black, Pawn,   Square(f, `_4`), true ) => pawnCaptures(Black) ++ enPassants(f, -1) :+ pawnAdvance((0, -1), 1)
        case Piece(Black, Pawn,   _,               true ) => pawnCaptures(Black) :+ pawnAdvance((0, -1), 1)
        case Piece(_,     Rook,   _,               _    ) => file ++ rank
        case Piece(_,     Knight, _,               _    ) => Vector(path((2, 1)), path((2, -1)), path((1, 2)), path((1, -2)), path((-2, 1)), path((-2, -1)), path((-1, 2)), path((-1, -2)))
        case Piece(_,     Bishop, _,               _    ) => diagonals
        case Piece(White, King,   E1,              false) => adjacent ++ castles(White, _1)
        case Piece(Black, King,   E8,              false) => adjacent ++ castles(Black, _8)
        case Piece(_,     King,   _,               _    ) => adjacent
        case Piece(_,     Queen,  _,               _    ) => file ++ rank ++ diagonals
      }
      all.filterNot(_.isEmpty)

    }

    def moves(start: Square, end: Square, pieceType: PieceType): Vector[_ <: Move] = {

      def promotions(starts: Vector[Square], rankOffset: Int) = for {
        start <- starts
        fileOffset <- Vector(-1, 0, 1)
        promotionType <- PromotionType.all
        end = start + (fileOffset, rankOffset)
        if end.isDefined
      } yield Promotion(start, end.get, promotionType)
      val allPromotions = promotions(files.map(square(_,_7)), 1) ++ promotions(files.map(square(_,_2)), -1)
      val promotion = new PartialFunction[(Square, Square, PieceType), Vector[Promotion]] {
        override def isDefinedAt(args: (Square, Square, PieceType)): Boolean = args._3 == Pawn && allPromotions.exists(p => args._1 == p.start && args._2 == p.end)
        override def apply(args: (Square, Square, PieceType)): Vector[Promotion] = PromotionType.all.map(t => Promotion(args._1, args._2, t))
      }

      val allCastles = Vector(whiteKingside, whiteQueenside, blackKingside, blackQueenside)
      val castle = new PartialFunction[(Square, Square, PieceType), Vector[Castle]] {
        override def isDefinedAt(args: (Square, Square, PieceType)): Boolean = args._3 == King && allCastles.exists(c => args._1 == c.start && args._2 == c.end)
        override def apply(args: (Square, Square, PieceType)): Vector[Castle] = allCastles.filter(c => args._1 == c.start && args._2 == c.end).map(_.castle)
      }

      def simple(args: (Square, Square, PieceType)) = Vector(SimpleMove(args._1, args._2))

      promotion.orElse(castle).applyOrElse((start, end, pieceType), simple)

    }

    def subpath(p: Vector[Square], s: Square): Vector[Square] = p.takeWhile(s !=)

    val (sameSide, otherSide) = pieces.partition(_.side == turn)
    val all: Vector[(Piece, Vector[Vector[Square]])] = sameSide.map(p => (p, paths(p)))
    val king: Square = sameSide.find(_.pieceType == King).get.square
    val absolutePins: Vector[Vector[Square]] = otherSide.map(paths(_, endOnPiece = false).filter(_.contains(king))).flatten
    def isBlockingCheck(p: Vector[Square], s: Square): Boolean = subpath(p, king).contains(s)
    val noAbsolutePins: Vector[(Piece, Vector[Vector[Square]])] = all.filterNot { case (piece, _) => absolutePins.exists(isBlockingCheck(_, piece.square)) }
    def checksFor(k: Square): Vector[Vector[Square]]  = otherSide.flatMap(paths(_).filter(_.contains(k)))
    def toMoves(args: (Piece, Vector[Vector[Square]])): Vector[_ <: Move] = args._2.flatMap(p => p.flatMap(end => moves(args._1.square, end, args._1.pieceType)))
    val doesNotEndInCheck = noAbsolutePins.map { case (piece, paths) =>
      if (piece.pieceType == King) (piece, paths.map(_.filter(checksFor(_).isEmpty)))
      else (piece, paths)
    }
    val checks = checksFor(king)
    (if (checks.isEmpty)
      doesNotEndInCheck
    else {
      def endsCheck(end: Square): Boolean = checks.forall(isBlockingCheck(_, end))
      doesNotEndInCheck.map { case (piece, paths) => (piece, paths.map(_.filter(endsCheck))) }
    }).flatMap(toMoves)

  }

  def isCheckmate = moves.isEmpty

  private def oneMove(nextTurn: Side, start: Square, end: Square, movePiece: Piece => Piece) = {
    val piece = pieceAt(start).get
    val enPassantTarget = piece match {
      case Piece(White, Pawn, Square(f, `_2`), _) if end == square(f, `_4`) => Some(f)
      case Piece(Black, Pawn, Square(f, `_7`), _) if end == square(f, `_5`) => Some(f)
      case _ => None
    }
    Some(Board(nextTurn, pieces.filterNot(_.square == start).filterNot(_.square == end) :+ movePiece(piece), enPassantTarget))
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
