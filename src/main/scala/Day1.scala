import scala.io.Source

object Day1 {

  def solve(lines: List[String], n: Int): AnyVal = {
    lines.map(_.toInt)
      .sorted
      .combinations(n)
      .dropWhile(_.sum != 2020)
      .take(1)
      .flatten
      .product
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input1.txt").getLines.toList

    println("Part1: " + solve(lines, 2))
    println("Part2: " + solve(lines, 3))
  }
}
