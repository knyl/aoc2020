import scala.annotation.tailrec
import scala.io.Source

object Day11 {
  @tailrec
  def parse(lines: List[String], map: Map[(Int, Int), Char] = Map.empty, y: Int = 0): Map[(Int, Int), Char] = {
    if (lines.isEmpty)
      return map

    val newMap = lines.head.indices.map(i => (i, y) -> lines.head(i)).toMap
    parse(lines.tail, map ++ newMap, y + 1)
  }

  def printMap(map: Map[(Int, Int), Char]): Unit = {
    val xMax = map.keys.map { case (x, _) => x }.max
    val yMax = map.keys.map { case (_, y) => y }.max
    for (y <- 0 to yMax) {
      for (x <- 0 to xMax) {
        print(map(x, y))
      }
      println()
    }
    println()
  }

  @tailrec
  def solve(map: Map[(Int, Int), Char], neighbourFun: (Int, Int, Map[(Int, Int), Char]) => List[(Int, Int)], maxNeighbours: Int, lastState: Set[(Int, Int)] = Set.empty): Int = {
    val updatedMap = map.keys.map { case (x, y) => nextState(x, y, map, neighbourFun, maxNeighbours) }.toMap
    val keySet = updatedMap.filter { case (_, c) => c == '#' }.keys.toSet
    if (keySet == lastState)
      updatedMap.count { case (_, c) => c == '#' }
    else
      solve(updatedMap, neighbourFun, maxNeighbours, keySet)
  }

  def nextState(x: Int, y: Int, map: Map[(Int, Int), Char], neighbourFun: (Int, Int, Map[(Int, Int), Char]) => List[(Int, Int)], maxNeighbours: Int): ((Int, Int), Char) = {
    val neighbours = neighbourFun(x, y, map)
    map(x, y) match {
      case 'L' => toOccupied(x, y, neighbours, map)
      case '#' => toFree(x, y, maxNeighbours, neighbours, map)
      case '.' => (x, y) -> '.'
    }
  }

  def toOccupied(x: Int, y: Int, neighbours: List[(Int, Int)], map: Map[(Int, Int), Char]): ((Int, Int), Char) = {
    val allNeighboursFree = !neighbours.exists { case (x, y) => map.getOrElse((x, y), '.') == '#' }
    if (allNeighboursFree)
      (x, y) -> '#'
    else
      (x, y) -> 'L'
  }

  def toFree(x: Int, y: Int, maxNeighbours: Int, neighbours: List[(Int, Int)], map: Map[(Int, Int), Char]): ((Int, Int), Char) = {
    val occupiedNeighbours = neighbours.count { case (x, y) => map.getOrElse((x, y), '.') == '#' }
    if (occupiedNeighbours >= maxNeighbours)
      (x, y) -> 'L'
    else
      (x, y) -> '#'
  }

  def getNeighbours(x: Int, y: Int, map: Map[(Int, Int), Char]): List[(Int, Int)] = {
    List((x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
      (x, y - 1), (x, y + 1),
      (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)).filter(map.contains)
  }

  def getNeighbours2(x: Int, y: Int, map: Map[(Int, Int), Char]): List[(Int, Int)] = {
    List(find(x, y, -1, -1, map), find(x, y, -1, 0, map), find(x, y, -1, 1, map),
      find(x, y, 0, -1, map), find(x, y, 0, 1, map),
      find(x, y, 1, -1, map), find(x, y, 1, 0, map), find(x, y, 1, 1, map)).flatten
  }

  @tailrec
  def find(x: Int, y: Int, dx: Int, dy: Int, map: Map[(Int, Int), Char]): Option[(Int, Int)] = {
    val nextX = x + dx
    val nextY = y + dy
    if (map.contains((nextX, nextY))) {
      map((nextX, nextY)) match {
        case '.' => find(nextX, nextY, dx, dy, map)
        case _ => Option((nextX, nextY))
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
