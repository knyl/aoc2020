import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  case class Pos(x: Int, y: Int, z: Int = 0, w: Int = 0) {
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y, z + that.z, w + that.w)
  }

  @tailrec
  private def parse(lines: List[String], map: Seq[Pos] = List.empty, y: Int = 0): Seq[Pos] = {
    if (lines.isEmpty)
      return map

    val newMap = lines.head.indices.flatMap(x => if (lines.head(x) == '#') Some(Pos(x, y)) else None)
    parse(lines.tail, map ++ newMap, y + 1)
  }

  private def getNeighbourPos(pos: Pos) = {
    val nums = -1 to 1
    val neighbours = for (x <- nums; y <- nums; z <- nums) yield Pos(x, y, z) + pos
    neighbours.filter(_ != pos)
  }

  private def getNeighbourPos2(pos: Pos) = {
    val nums = -1 to 1
    val neighbours = for (x <- nums; y <- nums; z <- nums; w <- nums) yield Pos(x, y, z, w) + pos
    neighbours.filter(_ != pos)
  }

  private def getNextCubeState(cube: Pos, state: Set[Pos], neighbourFun: Pos => Seq[Pos]) = {
    val activeNeighbours = neighbourFun(cube).count(state.contains)
    if ((state.contains(cube) && Set(2, 3).contains(activeNeighbours)) || (!state.contains(cube) && activeNeighbours == 3))
      Some(cube)
    else
      None
  }

  @tailrec
  private def solve(state: Set[Pos], maxRounds: Int, neighbourFun: Pos => Seq[Pos], round: Int = 0): Int = {
    if (maxRounds == round)
      return state.size
    val positionsWithNeighbours = state.flatMap(neighbourFun) union state
    val nextState = positionsWithNeighbours.flatMap(getNextCubeState(_, state, neighbourFun))
    solve(nextState, maxRounds, neighbourFun, round + 1)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input17.txt").getLines.toList
    val initialState = parse(lines)
    println("Part1: " + solve(initialState.toSet, 6, getNeighbourPos))
    println("Part2: " + solve(initialState.toSet, 6, getNeighbourPos2))
  }
}
