import scala.annotation.tailrec
import scala.io.Source

object Day20 {

  case class Tile(id: Int, tile: List[String] = List.empty)

  private def parse(lines: Seq[String], tiles: List[Tile] = List.empty) = lines.headOption match {
    case None => tiles
    case Some(s"Tile $id:") => parseTile(lines.tail, tiles, Tile(id.toInt))
  }

  @tailrec
  private def parseTile(lines: Seq[String], tiles: List[Day20.Tile], tile: Day20.Tile): List[Tile] = lines.headOption match {
    case None => Tile(tile.id, tile.tile.reverse) :: tiles
    case Some("") => parse(lines.tail, Tile(tile.id, tile.tile.reverse) :: tiles)
    case Some(str) => parseTile(lines.tail, tiles, Tile(tile.id, str :: tile.tile))
  }

  private def solve1(tiles: List[Day20.Tile]): BigInt = {
    val tilesWithEdges = tiles.map(tile => tile.id -> getEdgeRows(tile.tile))
    tilesWithEdges.map(entry => (countMatches(entry, tilesWithEdges), entry._1))
      .filter { case (count, _) => count == 2 }
      .map(_._2)
      .map(BigInt(_))
      .product
  }

  private def countMatches(tuple: (Int, List[String]), tiles: List[(Int, List[String])]) = {
    tiles.count { case (key, lines) => tuple._1 != key && lines.exists(tuple._2.contains(_)) }
  }

  private def getEdgeRows(tile: List[String]) = {
    val list = List(tile.head, tile.last, tile.map(_.head).mkString, tile.map(_.last).mkString)
    list ++ list.map(_.reverse)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input20.txt").getLines.toList
    val tiles = parse(lines)
    println("Part1: :" + solve1(tiles))
  }
}
