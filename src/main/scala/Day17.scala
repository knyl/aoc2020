import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  case class Pos(x: Int, y: Int, z: Int, w: Int)

  def printMap(map: Map[Pos, Boolean]): Unit = {
    val xMax = map.keys.map(_.x).max
    val xMin = map.keys.map(_.x).min
    val yMax = map.keys.map(_.y).max
    val yMin = map.keys.map(_.y).min
    val zMax = map.keys.map(_.z).max
    val zMin = map.keys.map(_.z).min
    for (z <- zMin to zMax) {
      println("Level: " + z)

      for (y <- yMin to yMax) {
        for (x <- xMin to xMax) {
          val c = if (map(Pos(x, y, z, 0))) '#' else '.'
          print(c)
        }
        println()
      }
      println()
    }
    println()
  }

  @tailrec
  private def parse(lines: List[String], map: Map[Pos, Boolean] = Map.empty, y: Int = 0): Map[Pos, Boolean] = {
    if (lines.isEmpty)
      return map

    val newMap = lines.head.indices.map(i => Pos(i, y, 0, 0) -> (lines.head(i) == '#')).toMap
    parse(lines.tail, map ++ newMap, y + 1)
  }

  private def getActiveNeighbours(pos: Pos, state: Map[Pos, Boolean]) = {
    val n = getNeighbourPos(pos).map(state.getOrElse(_, false)).count(_ == true)
    n
  }

  private def getNeighbourPos(pos: Pos) = {
    val n = List.tabulate(3, 3, 3, 3)((i, j, k, l) => (pos.x - 1 + i, pos.y - 1 + j, pos.z - 1 + k, pos.w - 1 + l)).flatten.flatten.flatten.map { case (x, y, z, w) => Pos(x, y, z, w) }
    n.filter(_ != pos)
  }

  private def getNextCubeState(cube: (Pos, Boolean), state: Map[Pos, Boolean]) = {
    val activeNeighbours = getActiveNeighbours(cube._1, state)
    if (cube._2)
      activeNeighbours >= 2 && activeNeighbours <= 3
    else
      activeNeighbours == 3
  }

  // This should only fetch the outer shell of positions, but let's try this first since it's easier
  private def getAllPositionsOneLayerOut(positions: Iterable[Pos]) = {
    val xMin = positions.minBy(_.x).x - 1
    val xMax = positions.maxBy(_.x).x + 1
    val yMin = positions.minBy(_.y).y - 1
    val yMax = positions.maxBy(_.y).y + 1
    val zMin = positions.minBy(_.z).z - 1
    val zMax = positions.maxBy(_.z).z + 1
    val wMin = positions.minBy(_.w).w - 1
    val wMax = positions.maxBy(_.w).w + 1
    (wMin to wMax).flatMap(w => (zMin to zMax).flatMap(z => (yMin to yMax).flatMap(y => (xMin to xMax).map(x => Pos(x, y, z, w))))).toList
  }

  @tailrec
  private def solve1(state: Map[Pos, Boolean], maxRounds: Int, round: Int = 0): Int = {
    if (maxRounds == round)
      return state.values.count(_ == true)
    val shellPositions = getAllPositionsOneLayerOut(state.keys)
    val updatedState = shellPositions.foldLeft(state)((acc, pos) => if (acc.contains(pos)) acc else acc + (pos -> false))
    val nextState = updatedState.foldLeft(Map.empty: Map[Pos, Boolean])((s, cube) => s + (cube._1 -> getNextCubeState(cube, updatedState)))
    solve1(nextState, maxRounds, round + 1)
  }


  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input17.txt").getLines.toList
    val initialState = parse(lines)
    println("Part1: " + solve1(initialState, 6))
  }

}
