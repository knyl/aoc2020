import scala.annotation.tailrec
import scala.io.Source

object Day12 {

  case class Instruction(action: String, data: Int)
  case class State(direction: String, pos: (Int, Int))

  private def parse(line: String): Instruction = {
    val pattern = """([A-Z])(\d+)""".r
    val pattern(action, data) = line
    Instruction(action, data.toInt)
  }

  private def distance(start: (Int, Int), end: (Int, Int)): Int = {
    (end._1 - start._1).abs + (end._2 - start._2).abs
  }

  @tailrec
  private def solve1(instructions: List[Instruction], state: State = State("E", (0, 0))): (Int, Int) = {
    if (instructions.isEmpty)
      return state.pos
    val instruction = instructions.head
    instruction.action match {
      case "F" => solve1(instructions.tail, State(state.direction, move(state.pos, instruction.data, state.direction)))
      case "E" => solve1(instructions.tail, State(state.direction, move(state.pos, instruction.data, instruction.action)))
      case "W" => solve1(instructions.tail, State(state.direction, move(state.pos, instruction.data, instruction.action)))
      case "N" => solve1(instructions.tail, State(state.direction, move(state.pos, instruction.data, instruction.action)))
      case "S" => solve1(instructions.tail, State(state.direction, move(state.pos, instruction.data, instruction.action)))
      case "L" => solve1(instructions.tail, turn(state, instruction))
      case "R" => solve1(instructions.tail, turn(state, instruction))
    }
  }

  @tailrec
  private def solve2(instructions: List[Instruction], state: State = State("E", (0, 0)), waypoint: (Int, Int) = (10, 1)): (Int, Int) = {
    if (instructions.isEmpty)
      return state.pos
    val instruction = instructions.head
    instruction.action match {
      case "F" => solve2(instructions.tail, forward(state, instruction, waypoint), waypoint)
      case "E" => solve2(instructions.tail, state, move(waypoint, instruction.data, instruction.action))
      case "W" => solve2(instructions.tail, state, move(waypoint, instruction.data, instruction.action))
      case "N" => solve2(instructions.tail, state, move(waypoint, instruction.data, instruction.action))
      case "S" => solve2(instructions.tail, state, move(waypoint, instruction.data, instruction.action))
      case "L" => solve2(instructions.tail, state, turn(instruction, waypoint))
      case "R" => solve2(instructions.tail, state, turn(instruction, waypoint))
    }
  }


  private def turn(state: State, instruction: Instruction): State = {
    instruction.action match {
      case "R" => State(newDirectionRight(state.direction, instruction.data), state.pos)
      case "L" => State(newDirectionLeft(state.direction, instruction.data), state.pos)
    }
  }

  private def newDirectionRight(str: String, i: Int): String = {
    val turns = i / 90
    (1 to turns).foldLeft(str)((acc, _) => turnRight(acc))
  }

  private def newDirectionLeft(str: String, i: Int): String = {
    val turns = i / 90
    (1 to turns).foldLeft(str)((acc, _) => turnLeft(acc))
  }

  private def turnLeft(str: String): String = {
    str match {
      case "E" => "N"
      case "N" => "W"
      case "W" => "S"
      case "S" => "E"
    }
  }

  private def turnRight(str: String): String = {
    str match {
      case "E" => "S"
      case "N" => "E"
      case "W" => "N"
      case "S" => "W"
    }
  }

  private def turn(instruction: Instruction, waypoint: (Int, Int)): (Int, Int) = {
    val turns = instruction.data / 90
    instruction.action match {
      case "R" => (1 to turns).foldLeft(waypoint)((w, _) => (w._2, -w._1))
      case "L" => (1 to turns).foldLeft(waypoint)((w, _) => (-w._2, w._1))
    }
  }

  private def forward(state: State, instruction: Instruction, waypoint: (Int, Int)) = {
    State(state.direction, (state.pos._1 + waypoint._1 * instruction.data, state.pos._2 + waypoint._2 * instruction.data))
  }

  private def move(state: (Int, Int), movement: Int, action: String): (Int, Int) = {
    action match {
      case "E" => (state._1 + movement, state._2)
      case "W" => (state._1 - movement, state._2)
      case "N" => (state._1, movement + state._2)
      case "S" => (state._1, state._2 - movement)
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = Source.fromResource("input12.txt").getLines.map(parse).toList
    println("Part1: " + distance((0, 0), solve1(instructions)))
    println("Part2: " + distance((0, 0), solve2(instructions)))
  }
}
