
import scala.annotation.tailrec
import scala.io.Source

object Day16 {

  @tailrec
  private def parseFields(lines: List[String], numbers: Map[String, List[Int]] = Map.empty): (Map[String, List[Int]], List[String]) = {
    if (lines.head.contains("your ticket"))
      return (numbers, lines.tail)

    val pattern = """([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
    val pattern(word, n1, n2, n3, n4) = lines.head
    val ns = Range(n1.toInt, n2.toInt+1) ++ Range(n3.toInt, n4.toInt+1)
    val updatedNumbers = numbers + (word -> ns.toList)
    parseFields(lines.tail, updatedNumbers)
  }

  @tailrec
  private def parseMyTicket(lines: List[String], numbers: List[Int] = List.empty): (List[Int], List[String]) = {
    if (lines.head.contains("nearby"))
      return (numbers, lines.tail)

    val updatedNumbers = numbers.appendedAll(lines.head.split(",").map(_.toInt))
    parseMyTicket(lines.tail, updatedNumbers)
  }

  @tailrec
  private def parseOtherTickets(lines: List[String], numbers: List[List[Int]] = List.empty): List[List[Int]] = {
    if (lines.isEmpty)
      return numbers

    val newNumbers = lines.head.split(",").map(_.toInt).toList
    parseOtherTickets(lines.tail, newNumbers :: numbers)
  }

  private def solve2(validNumbers: Map[String, List[Int]], myTicket: List[Int], otherTickets: List[List[Int]]) = {
    val validNumberSet = validNumbers.values.foldLeft(Set.empty: Set[Int])(_ union _.toSet)
    val validOtherTickets = otherTickets.filter(_.forall(validNumberSet.contains))

    val labels = (myTicket :: validOtherTickets).map(list => list.map(findMatchingLabel(_, validNumbers)))
    val labels2 = labels.map(_.zipWithIndex)
    val labels3 = labels2.flatten
    val allLabels = validNumbers.keySet
    val labels4 = labels3.foldLeft(Map.empty: Map[Int, Set[String]])((acc, el) => acc + (el._2 -> (el._1 intersect acc.getOrElse(el._2, allLabels))))
    val labels5 = labels4.toList.sortBy(_._2.size)
    val labels6 = assignLabels(labels5)

    val departureKeys = labels6.filter(_._1.startsWith("departure")).values
    departureKeys.map(myTicket(_)).map(_.toLong).product
  }

  @tailrec
  private def assignLabels(labels: List[(Int, Set[String])], assignedLabels: Set[String] = Set.empty, assignedPositions: Map[String, Int] = Map.empty): Map[String, Int] = {
    if (labels.isEmpty)
      return assignedPositions

    val possibleLabel = (labels.head._2 diff assignedLabels).head
    assignLabels(labels.tail, assignedLabels + possibleLabel, assignedPositions + (possibleLabel -> labels.head._1))
  }

  private def findMatchingLabel(num: Int, validNumbers: Map[String, List[Int]]) = {
    validNumbers.filter(_._2.contains(num)).keySet
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input16.txt").getLines.toList
    val (validNumbers, remLines) = parseFields(lines)
    val (myTicket, remLines2) = parseMyTicket(remLines)
    val ticketNumbers = parseOtherTickets(remLines2)

    val validNumberSet = validNumbers.values.foldLeft(Set.empty: Set[Int])(_ union _.toSet)
    val invalidNumbers = ticketNumbers.flatten.filterNot(n => validNumberSet.contains(n))
    println("Part1: " + invalidNumbers.sum)
    println("Part2: " + solve2(validNumbers, myTicket, ticketNumbers))
  }

}
