package mateinone

// A chess clock with each side's time left in ms, and the increment added after each of its
// moves. The time since the last move counts against the side to move.
class ChessClock(initial: Long, val increment: Long, now: () => Long = () => System.nanoTime() / 1000000) {
  private val left = Array(initial, initial)
  private var turnStart = now()

  def timeLeft(side: Int, sideToMove: Int): Long =
    if (side == sideToMove) left(side) - (now() - turnStart) else left(side)

  // Ends the side's turn: the time since the last move is spent, and the increment added.
  def moved(side: Int): Unit = {
    val t = now()
    left(side) += increment - (t - turnStart)
    turnStart = t
  }
}

object ChessClock {
  // m:ss, rounded up, so that only an empty clock shows 0:00.
  def format(ms: Long): String = {
    val s = (Math.max(ms, 0) + 999) / 1000
    f"${s / 60}:${s % 60}%02d"
  }
}
