import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day7 {
  def solve1(lines: List[String], bag: String): Int = {
    val graph = inputToGraph(lines, Map.empty)
    val visited = bfs(Queue(bag), Set.empty, graph)
    visited.size - 1
  }

  @tailrec
  def bfs(queue: Queue[String], visited: Set[String], graph: Map[String, List[String]]): Set[String] = {
    if (queue.isEmpty)
      visited
    else {
      val bags = graph.getOrElse(queue.head, List.empty)
      bfs(queue.tail ++ bags, visited + queue.head, graph)
    }
  }

  @tailrec
  def inputToGraph(value: List[String], graph: Map[String, List[String]]):  Map[String, List[String]] = {
    if (value.isEmpty)
      return graph
    val pattern = """(.*) bags contain (no other bags|.*)\.""".r
    value.head match {
      case pattern(_, "no other bags") => inputToGraph(value.tail, graph)
      case pattern(bag, bagsString) =>
        val bags = bagsString
          .split(",")
          .map(_.replaceAll("""\d|bags|bag""", ""))
          .map(_.trim)
        val updatedGraph = bags.foldLeft(graph)((g, b) =>  g + (b -> (bag :: graph.getOrElse(b, List.empty))))
        inputToGraph(value.tail, updatedGraph)
    }
  }

  def solve2(lines: List[String], bag: String): Int = {
    val graph = inputToGraph2(lines, Map.empty)
    bfs2(0, Queue((bag, 1)), graph)
  }

  @tailrec
  def bfs2(count: Int, queue: Queue[(String, Int)], graph: Map[String, List[(String, Int)]]): Int = {
    if (queue.isEmpty)
      count
    else {
      val bagsAndData = graph.getOrElse(queue.head._1, List.empty)
      val multiplier = queue.head._2
      val bags = bagsAndData.map(a => (a._1, a._2 * multiplier))
      val bagCount = bags.map(a => a._2).sum
      bfs2(count + bagCount, queue.tail ++ bags, graph)
    }
  }

  @tailrec
  def inputToGraph2(value: List[String], graph: Map[String, List[(String, Int)]]): Map[String, List[(String, Int)]] = {
    if (value.isEmpty)
      return graph
    val pattern = """(.*) bags contain (no other bags|.*)\.""".r
    value.head match {
      case pattern(_, "no other bags") => inputToGraph2(value.tail, graph)
      case pattern(bag, bagsString) =>
        val p2 = """(\d+) ([a-zA-Z ]+)""".r
        val bags = bagsString
          .split(",")
          .map(_.replaceAll("bags|bag", ""))
          .map(_.trim)
          .map(s => {
            val p2(num, color) = s
            (color.trim, num.trim.toInt)
          })
          .toList
        inputToGraph2(value.tail, graph + (bag -> bags))
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input7.txt").getLines.toList
    println("Part1: " + solve1(lines, "shiny gold"))
    println("Part2: " + solve2(lines, "shiny gold"))
  }
}
