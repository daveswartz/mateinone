package mateinone

object OccupiedPath {
  type Path = List[Square]
}
import OccupiedPath.Path

case class OccupiedPath(private val path: Path, private val occupied: Set[Square]) {
  def contains(s: Square): Boolean = path.contains(s)
  def vacate(s: Set[Square]): OccupiedPath = copy(occupied = this.occupied -- s)
  def occupy(s: Set[Square]): OccupiedPath = copy(occupied = this.occupied ++ s.filter(contains))
  val (beforeFirstOccupied, firstOccupied, secondOccupied) =
    if (occupied.isEmpty) (path, None, None)
    else {
      val sorted = occupied.toList.sortWith((a, b) => path.indexOf(a) < path.indexOf(b))
      val before = path.takeWhile(!occupied.contains(_))
      sorted match {
        case first :: second :: _ => (before, Some(first), Some(second))
        case first :: _ => (before, Some(first), None)
      }
    }
}
