import scala.annotation.tailrec
import scala.io.Source

object Day11 {

  case class Coord(x: Int, y: Int)
  type NMap = Map[Coord, Char]

  @tailrec
  def parse(lines: List[String], map: NMap = Map.empty, y: Int = 0): NMap = {
    if (lines.isEmpty)
      return map

    val newMap = lines.head.indices.map(i => Coord(i, y) -> lines.head(i)).toMap
    parse(lines.tail, map ++ newMap, y + 1)
  }

  def printMap(map: NMap): Unit = {
    val xMax = map.keys.map(_.x).max
    val yMax = map.keys.map(_.y).max
    for (y <- 0 to yMax) {
      for (x <- 0 to xMax) {
        print(map(Coord(x, y)))
      }
      println()
    }
    println()
  }

  @tailrec
  def solve(map: NMap, neighbourFun: (Coord, NMap) => List[Coord], maxNeighbours: Int, lastState: Set[Coord] = Set.empty): Int = {
    val updatedMap = map.keys.map(nextState(_, map, neighbourFun, maxNeighbours)).toMap
    val keySet = updatedMap.filter { case (_, c) => c == '#' }.keys.toSet
    if (keySet == lastState)
      updatedMap.count { case (_, c) => c == '#' }
    else
      solve(updatedMap, neighbourFun, maxNeighbours, keySet)
  }

  def nextState(coord: Coord, map: NMap, neighbourFun: (Coord, NMap) => List[Coord], maxNeighbours: Int): (Coord, Char) = {
    val neighbours = neighbourFun(coord, map)
    map(coord) match {
      case 'L' => toOccupied(coord, neighbours, map)
      case '#' => toFree(coord, maxNeighbours, neighbours, map)
      case '.' => coord -> '.'
    }
  }

  def toOccupied(coord: Coord, neighbours: List[Coord], map: NMap): (Coord, Char) = {
    val allNeighboursFree = !neighbours.exists(map.getOrElse(_, '.') == '#')
    if (allNeighboursFree)
      coord -> '#'
    else
      coord -> 'L'
  }

  def toFree(coord: Coord, maxNeighbours: Int, neighbours: List[Coord], map: NMap): (Coord, Char) = {
    val occupiedNeighbours = neighbours.count(map.getOrElse(_, '.') == '#')
    if (occupiedNeighbours >= maxNeighbours)
      coord -> 'L'
    else
      coord -> '#'
  }

  def getNeighbours(coord: Coord, map: NMap): List[Coord] = {
    val x = coord.x
    val y = coord.y
    List(Coord(x - 1, y - 1), Coord(x - 1, y), Coord(x - 1, y + 1),
      Coord(x, y - 1), Coord(x, y + 1),
      Coord(x + 1, y - 1), Coord(x + 1, y), Coord(x + 1, y + 1)).filter(map.contains)
  }

  def getNeighbours2(coord: Coord, map: NMap): List[Coord] = {
    List(find(coord, -1, -1, map), find(coord, -1, 0, map), find(coord, -1, 1, map),
      find(coord, 0, -1, map), find(coord, 0, 1, map),
      find(coord, 1, -1, map), find(coord, 1, 0, map), find(coord, 1, 1, map)).flatten
  }

  @tailrec
  def find(coord: Coord, dx: Int, dy: Int, map: NMap): Option[Coord] = {
    val nextCoord = Coord(coord.x + dx, coord.y + dy)
    if (map.contains(nextCoord)) {
      map(nextCoord) match {
        case '.' => find(nextCoord, dx, dy, map)
        case _ => Option(nextCoord)
      }
    } else
      None
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input11.txt").getLines.toList
    val map = parse(lines)
    println("Part1: " + solve(map, getNeighbours, 4))
    println("Part2: " + solve(map, getNeighbours2, 5))
  }
}
