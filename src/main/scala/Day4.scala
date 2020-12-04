import scala.annotation.tailrec
import scala.io.Source

object Day4 {

  @tailrec
  def parseLines(parsedLines: List[String], currentLine: String, lines: List[String]): List[String] = {
    if (lines.isEmpty)
      currentLine :: parsedLines
    else if (lines.head.isBlank)
      parseLines(currentLine :: parsedLines, "", lines.tail)
    else
      parseLines(parsedLines, currentLine + " " + lines.head, lines.tail)
  }

  def toMap(line: String): Map[String, String] = {
    line.split(" ").map(toTuple).toMap
  }

  def toTuple(line: String): (String, String) = {
    val string = line.split(":")
    (string(0), string(1))
  }

  def isValid(data: Map[String, String]): Boolean = {
    data.size == 8 || data.size == 7 && !data.contains("cid")
  }

  def validate4Digits(string: String, low: Int, high: Int): Boolean = {
    string.trim.matches("""^\d\d\d\d$""") && between(string.toInt, low, high)
  }

  def validateHgt(string: String): Boolean = {
    val inPattern = """^(\d{2})(in)$""".r
    val cmPattern = """^(\d{3})(cm)$""".r
    if (cmPattern.matches(string)) {
      val cmPattern(num, _) = string
      between(num.toInt, 150, 193)
    } else if (inPattern.matches(string)) {
      val inPattern(num, _) = string
      between(num.toInt, 59, 76)
    } else
      false
  }
  def validateHcl(string: String): Boolean = {
    string.trim.matches("""^#[0-9a-f]{6}$""")
  }
  def validateEcl(string: String): Boolean = {
    string.trim.matches("""amb|blu|brn|gry|grn|hzl|oth""")
  }
  def validatePid(string: String): Boolean = {
    string.trim.matches("""^\d{9}$""")
  }
  def between(num: Int, low: Int, high: Int): Boolean = {
    num >= low && num <= high
  }

  def validData(data: Map[String, String]): Boolean = {
    validate4Digits(data.getOrElse("byr", ""), 1920, 2002) &&
      validate4Digits(data.getOrElse("iyr", ""), 2010, 2020) &&
      validate4Digits(data.getOrElse("eyr", ""), 2020, 2030) &&
      validateHgt(data.getOrElse("hgt", "")) &&
      validateHcl(data.getOrElse("hcl", "")) &&
      validateEcl(data.getOrElse("ecl", "")) &&
      validatePid(data.getOrElse("pid", ""))

  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input4.txt").getLines.toList
    val map = parseLines(List.empty, "", lines)
      .map(_.trim)
      .map(toMap)
    println("Part1: " + map.count(isValid))
    println("Part2: " + map.count(a => isValid(a) && validData(a)))
  }
}
