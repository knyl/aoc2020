import scala.io.Source

object Day2 {

  case class Data(low: Int, high: Int, char: Char, password: String)

  def parseString(line: String): Data = {
    val pattern = "([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)".r
    val pattern(low, high, char, password) = line
    Data(low.toInt, high.toInt, char.charAt(0), password)
  }

  def validPassword(data: Data): Boolean = {
    val count = data.password.count(_ == data.char)
    data.low <= count && data.high >= count
  }

  def validPassword2(data: Data): Boolean = {
    val c1Match = data.password.charAt(data.low - 1) == data.char
    val c2Match = data.password.charAt(data.high - 1) == data.char
    c1Match != c2Match
  }

  def main(args: Array[String]): Unit = {
    val data = Source.fromResource("input2.txt").getLines.map(parseString).toList

    println("Part1: " + data.count(validPassword))
    println("Part2: " + data.count(validPassword2))
  }

}
