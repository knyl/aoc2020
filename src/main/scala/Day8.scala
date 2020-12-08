import scala.annotation.tailrec
import scala.io.Source

object Day8 {

  case class Instruction(instr: String, data: Int)

  def toInstruction(line: String): Instruction = {
    val pattern = """(jmp|acc|nop) ([+\-\d]+)""".r
    val pattern(instruction, data) = line
    Instruction(instruction, data.toInt)
  }

  @tailrec
  def solve(pos: Int = 0, acc: Int = 0, lines: List[Instruction], visited: Set[Int] = Set.empty): (Int, Boolean) = {
    if (pos >= lines.length)
      return (acc, true)
    if (visited.contains(pos))
      return (acc, false)
    val Instruction(newInstr, data) = lines(pos)
    val newVisited = visited + pos
    newInstr match {
      case "nop" => solve(pos + 1, acc, lines, newVisited)
      case "acc" => solve(pos + 1, acc + data, lines, newVisited)
      case "jmp" => solve(pos + data, acc, lines, newVisited)
    }
  }

  def solve2(lines: List[Instruction], ind: Int = 0): Int = {
    if (lines(ind).instr == "acc")
      return solve2(lines, ind + 1)
    val updatedLines = lines.updated(ind, Instruction(swapOperation(lines(ind).instr), lines(ind).data))
    val res = solve(0, 0, updatedLines, Set.empty)
    if (res._2)
      res._1
    else
      solve2(lines, ind + 1)
  }

  def swapOperation(str: String): String = {
    str match {
      case "jmp" => "nop"
      case "nop" => "jmp"
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input8.txt").getLines.toList.map(toInstruction)

    println("Part1: " + solve(lines = lines)._1)
    println("Part2: " + solve2(lines))
  }
}
