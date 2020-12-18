import scala.annotation.tailrec
import scala.io.Source

object Day18 {
  private def parse(str: String) = {
    val res = parsingStuff(str.toCharArray)
    calculate(res)
  }

  @tailrec
  private def calculate(str: List[Char], acc: List[BigInt] = List.empty): BigInt = str.headOption match {
    case None => acc.head
    case Some(x) if x.isDigit => calculate(str.tail, BigInt(str.head.asDigit) :: acc)
    case Some('+') =>
      val t1 :: t2 :: tail = acc
      calculate(str.tail, (t1 + t2) :: tail)
    case Some('*') => ()
      val t1 :: t2 :: tail = acc
      calculate(str.tail, (t1 * t2) :: tail)
  }

  @tailrec
  private def parsingStuff(str: Seq[Char], stack: List[Char] = List.empty, acc: List[Char] = List.empty): List[Char] = str.headOption match {
    case None => (stack ++ acc).reverse
    case Some('(') => parsingStuff(str.tail, str.head :: stack, acc) // When parenthesis starts, just push it on the stack
    case Some(')') => // If parenthesis end, then it's time to take stuff from the stack, until we find start parenthesis
      val subExpr = stack.takeWhile(_ != '(')
      val restOfExpression = stack.dropWhile(_ != '(')
      if (restOfExpression.head == '(') // Forget about the start parenthesis, and put the expression in the accumulator
        parsingStuff(str.tail, restOfExpression.tail, subExpr.reverse ++ acc)
      else // We reached the end of the stack!
        parsingStuff(str.tail, List.empty, restOfExpression.head :: (subExpr.reverse ++ acc))
    case Some(x) if x.isDigit => parsingStuff(str.tail, stack, str.head :: acc) // A digit is just put in the accumulator
    case _ => // Handle operators!!
      val subExpr = stack.takeWhile(_ != '(')
      val restOfExpression = stack.dropWhile(_ != '(')
      parsingStuff(str.tail, str.head :: restOfExpression, subExpr.reverse ++ acc)
  }

  private def parse2(str: String) = {
    val res = parsingStuff2(str.toCharArray)
    calculate(res)
  }

  @tailrec
  private def parsingStuff2(str: Seq[Char], stack: List[Char] = List.empty, acc: List[Char] = List.empty): List[Char] = str.headOption match {
    case None => (stack.reverse ++ acc).reverse
    case Some('(') => parsingStuff2(str.tail, str.head :: stack, acc) // When parenthesis starts, just push it on the stack
    case Some(')') => // If parenthesis end, then it's time to take stuff from the stack, until we find start parenthesis
      val subExpr = stack.takeWhile(_ != '(')
      val restOfExpression = stack.dropWhile(_ != '(')
      if (restOfExpression.head == '(') // Forget about the start parenthesis, and put the expression in the accumulator
        parsingStuff2(str.tail, restOfExpression.tail, subExpr.reverse ++ acc)
      else // We reached the end of the stack!
        parsingStuff2(str.tail, List.empty, restOfExpression.head :: (subExpr.reverse ++ acc))
    case Some(x) if x.isDigit => parsingStuff2(str.tail, stack, str.head :: acc) // A digit is just put in the accumulator
    case Some('+') => // Handle +!! with precedence this time >_<
      val subExpr = stack.takeWhile(i => i != '(' && i != '*') // also stop grabbing stuff if '*' turns up
      val restOfExpression = stack.dropWhile(i => i != '(' && i != '*')
      parsingStuff2(str.tail, str.head :: restOfExpression, subExpr.reverse ++ acc)
    case Some('*') => // Handle *!! with precedence this time >_<
      val subExpr = stack.takeWhile(_ != '(')
      val restOfExpression = stack.dropWhile(_ != '(')
      parsingStuff2(str.tail, str.head :: restOfExpression, subExpr.reverse ++ acc)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("input18.txt").getLines.map(_.replace(" ", "")).toList
    val value = lines.map(parse)
    val value2 = lines.map(parse2)
    println("Part1: " + value.sum)
    println("Part2: " + value2.sum)
  }
}
