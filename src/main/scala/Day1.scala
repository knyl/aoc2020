import scala.io.Source

object Day1 {

  def solve(lines: List[String], n: Int): AnyVal = {
    lines.map(l => Integer.parseInt(l))
      .sorted
      .combinations(n)
      .dropWhile(seq => seq.sum != 2020)
      .take(1)
      .toList
      .flatten
      .product
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input1.txt").getLines.toList

    val result1 = solve(lines, 2)
    val result2 = solve(lines, 3)
    println("Part1: " + result1)
    println("Part2: " + result2)
  }
}
