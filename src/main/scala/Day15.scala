import scala.annotation.tailrec

object Day15 {

  private def solve1(n: Int, numbers: List[Int]): Int = {
    val numberMap = numbers.zipWithIndex.map(tuple => (tuple._1, List(tuple._2 + 1))).toMap
    val round = numbers.length + 1
    val lastNum = numbers.last
    solve1(n, numberMap, round, lastNum)
  }

  @tailrec
  private def solve1(n: Int, numberMap: Map[Int, List[Int]], round: Int, lastNum: Int): Int = {
    //if (round % 1000 == 0)
    //  println("Round: $round lastnum: ")
    if (round == n) {
      return lastNum
    }
    if (numberMap.getOrElse(lastNum, List.empty).length == 1 | numberMap.getOrElse(lastNum, List.empty).isEmpty) {
      val spoken = 0
      val updatedList = (round :: numberMap.getOrElse(spoken, List.empty)).slice(0, 3)
      solve1(n, numberMap + (spoken -> updatedList), round + 1, spoken)
    }
    else {
      val spoken = numberMap(lastNum).head - numberMap(lastNum).tail.head
      val updatedList = (round :: numberMap.getOrElse(spoken, List.empty)).slice(0, 3)
      solve1(n, numberMap + (spoken -> updatedList), round + 1, spoken)
    }
  }

  def main(args: Array[String]): Unit = {
    val test1 = List(0,3,6) // 436, 175594
    val test2 = List(1,3,2) // 1, 2578
    val test3 = List(2,1,3) // 10, 3544142
    val test4 = List(1,2,3) // 27, 261214
    val test5 = List(2,3,1) // 78, 6895259
    val test6 = List(3,2,1) // 438, 18
    val test7 = List(3,1,2) // 1836, 362
    val input = List(19,0,5,1,10,13) // 1015, 201

    println("Part1: " + solve1(2021, input))
    println("Part2: " + solve1(30000001, input))
  }
}
