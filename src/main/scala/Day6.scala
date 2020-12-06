import scala.annotation.tailrec
import scala.io.Source

object Day6 {
  @tailrec
  def solve(reducer: (Set[Char], Set[Char]) => Set[Char], count: Int, currentLine: List[Set[Char]], lines: List[String]): Int = {
    if (lines.isEmpty)
      count + currentLine.reduce(reducer).size
    else if (lines.head.isBlank)
      solve(reducer, count + currentLine.reduce(reducer).size, List.empty, lines.tail)
    else
      solve(reducer, count, lines.head.toList.toSet :: currentLine, lines.tail)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input6.txt").getLines.toList

    println("Part1: " + solve((a, b) => a | b, 0, List.empty, lines))
    println("Part2: " + solve((a, b) => a & b, 0, List.empty, lines))
  }
}
