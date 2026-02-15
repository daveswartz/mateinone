package mateinone

object TranspositionTable {
  // Flag constants
  val Exact = 0      // We found the exact score (PV node)
  val LowerBound = 1 // Beta cutoff (Fail high) - Score is a lower bound
  val UpperBound = 2 // Alpha cutoff (Fail low) - Score is an upper bound

  // Entry structure: Depth, Score, Flag, BestMove
  case class Entry(depth: Int, score: Int, flag: Int, bestMove: Option[MoveBase])

  // Size = 2^20 (approx 1 million entries) to fit in memory easily
  private val Size = 1048576
  private val Mask = Size - 1
  
  private val table = new Array[Entry](Size)
  private val hashes = new Array[Long](Size) // Store full hash to verify collisions

  def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    java.util.Arrays.fill(hashes, 0L)
  }

  def get(hash: Long): Option[Entry] = {
    val index = (hash & Mask).toInt
    if (hashes(index) == hash) Option(table(index)) else None
  }

  // Always replace strategy or depth-preferred. 
  // Here we replace if the new entry is from a deeper or equal search, or if the table slot is empty.
  def store(hash: Long, depth: Int, score: Int, flag: Int, bestMove: Option[MoveBase]): Unit = {
    val index = (hash & Mask).toInt
    val existing = table(index)
    
    // Replacement scheme: Prefer deeper searches.
    if (existing == null || depth >= existing.depth) {
      hashes(index) = hash
      table(index) = Entry(depth, score, flag, bestMove)
    }
  }
}
