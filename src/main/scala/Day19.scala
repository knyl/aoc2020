import scala.annotation.tailrec
import scala.io.Source

object Day19 {

  private def formatRules(lines: List[String]) = {
    val ruleMap = lines.foldLeft(Map.empty: Map[String, String]){
      case (map, s"""$num: "$rule"""") => map + (num -> rule)
      case (map, s"$num: $rule") => map + (num -> rule)
    }.map{case (k, v) => k -> v.split("""\|""").toList }
      .map{case (k, v) => k -> v.map(_.trim.split(" ").toList)}

    val baseRules = ruleMap.filter(entryIsDone).map{case (k, v) => k -> v.flatten}
    populateRules(baseRules, ruleMap.filter(!entryIsDone(_)))
  }

  @tailrec
  private def populateRules(doneRules: Map[String, List[String]], rulesToBe: Map[String, List[List[String]]]): Map[String, List[String]] = {
    if (rulesToBe.isEmpty)
      return doneRules

    val updatedRulesToBe = rulesToBe.map{case (k, v) => k -> populateRule(v, doneRules)}
    val newDoneRules = updatedRulesToBe.filter(entryIsDone).map{case (k, v) => k -> v.flatten}
    populateRules(doneRules ++ newDoneRules, updatedRulesToBe.filter(!entryIsDone(_)))
  }

  private def populateRule(ruleToBe: List[List[String]], rules: Map[String, List[String]]): List[List[String]] = {
    if (ruleToBe.forall(_.forall(rules.contains))) {
      ruleToBe match {
        case (l11 :: l12 :: Nil) :: (l21 :: l22 :: Nil) :: Nil => // 109: 116 58 | 96 57
          val r1 = for (i <- rules(l11); j <- rules(l12)) yield i+j
          val r2 = for (i <- rules(l21); j <- rules(l22)) yield i+j
          List(r1 ++ r2)
        case (l1 :: l2 :: l3 :: Nil) :: Nil =>  // 0: 4 1 7
          val r = for (i <- rules(l1); j <- rules(l2); k <- rules(l3)) yield i+j+k
          List(r)
        case (l1 :: Nil) :: (l2 :: Nil) :: Nil => // 121: 58 | 57
          List(rules(l1), rules(l2))
        case (l1 :: l2 :: Nil) :: Nil => // 27: 57 30
          val r1 = for (i <- rules(l1); j <- rules(l2)) yield i+j
          List(r1)
        case (l1 :: Nil) :: Nil => // 8: 42
          List(rules(l1))
      }
    }
    else
      ruleToBe
  }

  private def entryIsDone(entry: (String, List[List[String]])) = entry match {
    case (_, value) => value.forall(_.forall(_.matches("[ab]+")))
  }
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input19.txt").getLines.toList
    val rules = formatRules(lines.takeWhile(_ != ""))
    val strings = lines.dropWhile(_ != "")
    val baseRule = rules("0").toSet
    println("Part 1: " + strings.count(baseRule.contains))

  }

}
