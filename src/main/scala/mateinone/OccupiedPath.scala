package mateinone

object OccupiedPath {
  type Path = List[Square]
}
import OccupiedPath.Path

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
