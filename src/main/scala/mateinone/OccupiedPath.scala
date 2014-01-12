package mateinone

object OccupiedPath {
  type Path = List[Square]
}
import OccupiedPath.Path

case class OccupiedPath(private val path: Path, private val occupied: Set[Square]) {
  def contains(s: Square): Boolean = path.contains(s)
  def vacate(s: Set[Square]): OccupiedPath = copy(occupied = this.occupied -- s)
  def occupy(s: Set[Square]): OccupiedPath = copy(occupied = this.occupied ++ s.filter(contains))
  val (beforeFirstOccupied, firstOccupiedAndAfter) =
    if (occupied.isEmpty) (path, List.empty[Square])
    else path.splitAt(occupied.map(o => path.indexOf(o)).min)
}
