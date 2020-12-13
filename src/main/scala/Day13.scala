import scala.io.Source

object Day13 {

  private def getEarliestTime(time: Int)(t: Int): Int = {
   t - (time % t)
  }

  private def solve1(lines: List[String]): Int = {
    val time = lines.head.toInt
    val departures = lines.last.split(",").filter(_ != "x").map(_.toInt).sorted
    val res = departures.map(i => (i, i - (time % i))).minBy(_._2)
    res._1 * res._2
  }

  /*
   * This method assumes that the input numbers are all prime numbers
   */
  private def solve2(lines: List[String]): String = {
    val departures = lines.last.split(",").zipWithIndex.filter(_._1 != "x").map(t => (t._1.toLong, t._2.toLong))
    val res = departures.tail.foldLeft((departures.head._1, 0.toLong))((acc, el) => (acc._1 * el._1, findNextValue(acc._1, acc._2, el)))
    res._2.toString
  }

  private def findNextValue(step: Long, start: Long, target: (Long, Long)): Long = {
    var currPos = start
    while (((currPos + target._2) % target._1) != 0 ) {
      currPos += step
    }
    currPos
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input13.txt").getLines.toList
    println("Part1: " + solve1(lines))
    println("Part2: " + solve2(lines))
  }
}
