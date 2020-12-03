import scala.io.Source

object Day3 {

  def countTrees(stepX: Int, stepY: Int, map: List[String]): Int = {
    val ys = Range(stepY, map.length, stepY)
    val xs = Range(stepX, ys.length * stepX + stepX, stepX)
    xs.zip(ys)
      .map(isTree(_, map))
      .count(_ == true)
  }

  def isTree(pos: (Int, Int), map: List[String]): Boolean = {
    val (x, y) = pos
    map(y).charAt(x % map.head.length) == '#'
  }

  def main(args: Array[String]): Unit = {
    val map = Source.fromResource("input3.txt").getLines.toList

    val p1 = countTrees(3, 1, map)
    println("Part1: " + p1)

    val p2 = p1 * countTrees(1, 1, map) * countTrees(5, 1, map) *
      countTrees(7, 1, map) * countTrees(1, 2, map)
    println("Part2: " + p2)
  }
}
