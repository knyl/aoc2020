import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day7 {
  def solve1(lines: List[String], bag: String): Int = {
    val (empty, graph) = parseData(lines, List.empty, Map.empty)
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
  def parseData(value: List[String], empty: List[String], graph: Map[String, List[String]]): (List[String], Map[String, List[String]]) = {
    if (value.isEmpty)
      return (empty, graph)

    val pattern = """(.*) bags contain (no other bags|.*)\.""".r
    value.head match {
      case pattern(bag, "no other bags") => parseData(value.tail, bag.trim :: empty, graph)
      case pattern(bag, bagsString) =>
        val bags = bagsString
          .split(",")
          .map(_.replaceAll("""\d""", ""))
          .map(_.replaceAll("bags", ""))
          .map(_.replaceAll("bag", ""))
          .map(_.trim)
        val updatedGraph = bags.foldLeft(graph)((g, b) =>  g + (b -> (bag :: graph.getOrElse(b, List.empty))))
        parseData(value.tail, empty, updatedGraph)
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input7.txt").getLines.toList
    println("Part1: " + solve1(lines, "shiny gold"))
  }
}
