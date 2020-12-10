import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  @tailrec
  def solve1(prev: Int, numbers: List[Int], ones: Int = 0, threes: Int = 1): Int = {
    if (numbers.isEmpty) {
      return ones * threes
    }
    numbers.head - prev match {
      case 1 => solve1(numbers.head, numbers.tail, ones + 1, threes)
      case 3 => solve1(numbers.head, numbers.tail, ones, threes + 1)
      case _ => solve1(numbers.head, numbers.tail, ones, threes)
    }
  }

  def solve2(numbers: List[Int]): Long = {
    val count = numbers.indices.foldLeft(List.empty : List[Long])((vals, i) => vals.appended(accumulateArrangements(numbers, vals, i)))
    count.last
  }

  private def accumulateArrangements(numbers2: List[Int], vals: List[Long], i: Int): Long = {
    val v = getArrangements(numbers2, vals, i, i - 1) + getArrangements(numbers2, vals, i, i-2) + getArrangements(numbers2, vals, i, i-3)
    if (v == 0)
      vals.lastOption.getOrElse(1)
    else
      v
  }

  def getArrangements(numbers: List[Int], vals: List[Long], i: Int, j: Int): Long =  {
    if (j < 0 || numbers(i) - numbers(j) > 3)
      0
    else
      vals(j)
  }

  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("input10.txt").getLines.map(_.toInt).toList.sorted
    println("Part1: " + solve1(0, numbers))
    println("Part2: " + solve2(0 :: numbers))
  }

}
