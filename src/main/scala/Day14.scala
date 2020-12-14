import scala.annotation.tailrec
import scala.io.Source

object Day14 {

  case class Data(floating: Set[Int], ones: Set[Int])

  private def solve1(input: List[String]) = {
    val data = input.tail.foldLeft((Map.empty: Map[Long, BigInt], toMask(input.head)))((acc, elem) => handle1(acc, elem))._1
    data.values.sum
  }

  private def toMask(str: String): List[(Long, Long)] = {
    val pattern = """mask = ([01X]+)""".r
    val pattern(mask) = str
    mask.reverse.zipWithIndex.map(a => (a._1, a._2.toLong)).filter(el => el._1 != 'X').map(el => (el._1.toString.toLong, el._2)).toList
  }

  private def handle1(acc: (Map[Long, BigInt], List[(Long, Long)]), line: String) = {
    line match {
      case s"mask = $_" => (acc._1, toMask(line))
      case s"mem[$num] = $data" => writeMem(acc, num, data)
    }
  }

  private def writeMem(tuple: (Map[Long, BigInt], List[(Long, Long)]), index: String, data: String) = {
    val map = tuple._1
    val mask = tuple._2
    val newData = applyMask(map, mask, index, data)
    (newData, mask)
  }

  private def applyMask(map: Map[Long, BigInt], mask: List[(Long, Long)], index: String, data: String): Map[Long, BigInt] = {
    map.updated(index.toLong, applyMask(mask, data.toLong))
  }

  private def applyMask(mask: List[(Long, Long)], num: BigInt): BigInt = {
   mask.foldLeft(num)(createMask)
  }

  private def createMask(num: BigInt, mask: (Long, Long)) = {
    mask match {
      case (1, ind) => num | (BigInt(1) << ind.toInt)
      case (0, ind) => num & ~(BigInt(1) << ind.toInt)
    }
  }

  private def solve2(input: List[String]) = {
    val res = input.tail.foldLeft((Map.empty: Map[BigInt, Int], toMask2(input.head)))((acc, el) => handle(acc, el))
    res._1.values.map(BigInt(_)).sum
  }

  private def handle(acc: (Map[BigInt, Int], Data), line: String) = {
    line match {
      case s"mask = $_" => (acc._1, toMask2(line))
      case s"mem[$num] = $data" => calculateMemPositions(acc, BigInt(num), data.toInt)
    }
  }

  private def calculateMemPositions(tuple: (Map[BigInt, Int], Data), int: BigInt, data: Int) = {
    val value = tuple._2.ones.foldLeft(int)((acc, el) => acc.setBit(el))
    val value2 = tuple._2.floating.foldLeft(List(value))((list, bit) => list.map(i => i.clearBit(bit)) ++ list.map(i => i.setBit(bit)))
    val res = value2.map(v => v -> data)
    (tuple._1 ++ res, tuple._2)
  }

  private def toMask2(str: String): Data = {
    val pattern = """mask = ([01X]+)""".r
    val pattern(mask) = str
    createMaskSet(mask.reverse.zipWithIndex.toList)
  }

  @tailrec
  private def createMaskSet(mask: List[(Char, Int)], list: Data = Data(Set.empty, Set.empty)): Data = {
    if (mask.isEmpty)
      return list
    mask.head match {
      case ('0', _) => createMaskSet(mask.tail, list)
      case ('1', ind) => createMaskSet(mask.tail, Data(ones = list.ones + ind, floating = list.floating))
      case ('X', ind) => createMaskSet(mask.tail, Data(ones = list.ones, floating = list.floating + ind))
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input14.txt").getLines.toList
    println("Part1: " + solve1(lines))
    println("Part2: " + solve2(lines))
  }
}