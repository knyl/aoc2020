import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day9 {

  @tailrec
  def solve1(numbers1: Queue[BigInt], numbers2: List[BigInt]): BigInt = {
    val n = numbers1.toList.combinations(2).map(_.sum).toSet
    if (n.contains(numbers2.head))
      solve1(numbers1.tail.enqueue(numbers2.head), numbers2.tail)
    else
      numbers2.head
  }

  def solve2(numbers: List[BigInt], num: BigInt): BigInt = {
    val maxInd = numbers.indexOf(num)
    val map = dp(numbers, Range(0, maxInd).toList)
    val (key, _) = map.find{case (_, value) => value == num}.head
    val slice = numbers.slice(key._1, key._2 + 1)
    slice.min + slice.max
  }

  @tailrec
  def dp(numbers: List[BigInt], indices: List[Int], data: Map[(Int, Int), BigInt] = Map.empty): Map[(Int, Int), BigInt] = {
    if (indices.isEmpty)
      return data
    val i = indices.head
    val updatedMap = indices.foldLeft(data)((map, j) => map + ((i, j) -> accumulateSum(numbers, map, i, j)))
    dp(numbers, indices.tail, updatedMap)
  }

  def accumulateSum(numbers: List[BigInt], map: Map[(Int, Int), BigInt], i: Int, j: Int): BigInt = {
    map.getOrElse((i, j - 1), BigInt(0)) + numbers(j)
  }

  def main(args: Array[String]): Unit = {
    val numbers = Source.fromResource("input9.txt").getLines.toList.map(BigInt(_))
    val (list1, list2) = numbers.splitAt(25)
    val res1 = solve1(Queue(list1: _*), list2)
    println("Part1: " + res1)
    println("Part2: " + solve2(numbers, res1))
  }
}
