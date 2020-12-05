import scala.annotation.tailrec
import scala.io.Source

object Day5 {

  def solve(input: String): Int = {
    val row = partition(0, 127, input.substring(0, 7))
    val column = partition(0, 8, input.substring(7, 10))
    row * 8 + column
  }

  @tailrec
  def partition(low: Int, high: Int, list: String): Int = {
    if (list.isEmpty)
      return low
    val diff = ((high - low).toDouble / 2).ceil.toInt
    if (list.head == 'F' || list.head == 'L') {
      partition(low, high-diff, list.tail)
    } else {
      partition(low+diff, high, list.tail)
    }
  }

  @tailrec
  def findSeat(currSeat: Int, seats: List[Int]): Int = {
    if (seats.head != currSeat + 1)
      currSeat + 1
    else
      findSeat(seats.head, seats.tail)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input5.txt").getLines.toList
    val seats = lines.map(solve)
    println("Part 1 " + seats.max)
    val sorted = seats.sorted
    println("Part 2 " + findSeat(sorted.head, sorted.tail))
  }
}
