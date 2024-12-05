package day05

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

object Day05 extends Exercise(2024, 5) {

  def run(input: List[String]): Unit = {
    part1(input)
    part2(input)
  }

  private def prepInputRules(input: List[String]): List[(Int, Int)] = {
    val filteredInput = input.filter(_.contains('|'))
    filteredInput.map { x =>
      val rule = x.split('|')
      (rule.head.toInt, rule.last.toInt)
    }
  }

  private def prepareRulesLkp(rules: List[(Int, Int)]): Map[Int, List[Int]] = {
    rules.groupBy(_._1).map { case (k, v) =>
      k -> v.map(_._2)
    }
  }

  private def prepInputOrder(input: List[String]): List[List[Int]] = {
    val filteredInput = input.filter(!_.contains('|'))
    filteredInput.filter(_.nonEmpty).map(_.split(",").map(_.toInt).toList)
  }

  // permutacja ale tylko wychodząca od początku
  private def createPermutation(order: List[Int]): List[List[Int]] = {
    order.indices.map(i => order.drop(i)).toList.filter(_.length > 1)
  }

  private def createAllPermutations(orders: List[List[Int]]): List[List[List[Int]]] = {
    orders.map(createPermutation)
  }

  private def checkIfOrdersAreCorrect(orderPerm: List[List[Int]], rulesLkp: Map[Int, List[Int]]): Boolean = {
    val orderPermCheck = orderPerm.map { x =>
      val key    = x.head
      val values = x.tail
      val lkp    = rulesLkp get key
      val lkpValue = lkp match
        case Some(list) => list
        case None       => List.empty
      values.forall(lkpValue.contains)
    }

    !orderPermCheck.contains(false) // false jak nie jest dobrze
  }

  private def checkIfOrdersAreCorrectSingleList(order: List[Int], rulesLkp: Map[Int, List[Int]]): Boolean = {
    val key    = order.head
    val values = order.tail
    val lkp    = rulesLkp get key
    val lkpValue = lkp match
      case Some(list) => list
      case None       => List.empty
    values.forall(lkpValue.contains)
  }

  private def checkIfTwoValuesAreCorrect(a: Int, b: Int, rulesLkp: Map[Int, List[Int]]): Boolean = {
    val lkp = rulesLkp get a
    lkp match
      case Some(list) => list.contains(b)
      case None       => false
  }

  private def findOnlyCorrectOrders(listOfCorrectness: List[Boolean], listOfOrders: List[List[Int]]): List[List[Int]] = {
    listOfOrders.filter { x => listOfCorrectness(listOfOrders.indexOf(x)) }
  }

  private def findOnlyWrongOrders(listOfCorrectness: List[Boolean], listOfOrders: List[List[Int]]): List[List[Int]] = {
    listOfOrders.filter { x => !listOfCorrectness(listOfOrders.indexOf(x)) }
  }

  private def findMiddleValue(order: List[Int]): Int = {
    order(order.length / 2)
  }

  // my solution - totally not working

//  def sortForCorrectness(singleOrder: List[Int], rulesLkp: Map[Int, List[Int]]): List[Int] = {
//    @tailrec
//    def helper(remaining: List[Int], acc: List[Int]): List[Int] = {
//      remaining match
//        case Nil => acc
//        case head :: Nil => acc :+ head
//        case head :: tail => {
//          println(acc)
//          if (checkIfTwoValuesAreCorrect(head, tail.head, rulesLkp)) {
//            helper(tail, acc :+ head)
//          } else if (checkIfTwoValuesAreCorrect(tail.head, head, rulesLkp)) {
//            helper(head :: tail.tail, acc :+ tail.head)
//          } else {
//            helper(head :: tail.tail, acc)
//          }
//        }
//    }
//    helper(singleOrder, List.empty)
//  }

  // part 2 - not my solution, but by spamegg1
  // I thought of it at first, but I was afraid of it XD.
  // It's a 2 pointer algorithm, simple swapping, didn't think it's possible in scala but apparently it is when you use Arrays lol
  private def fix(rules: List[(Int, Int)], order: Array[Int]): Array[Int] = {
    for (i <- 0 to order.length - 2) {
      for (j <- i + 1 until order.length) {
        if (rules.contains((order(j), order(i)))) {
          val temp = order(i)
          order(i) = order(j)
          order(j) = temp
        }
      }
    }
    order
  }

  private def part1(input: List[String]): Int = part1 {
    val rules             = prepInputRules(input)
    val order             = prepInputOrder(input)
    val rulesLkp          = prepareRulesLkp(rules)
    val permutations      = createAllPermutations(order)
    val listOfCorrectness = permutations.map(checkIfOrdersAreCorrect(_, rulesLkp))
    val correctOrders     = findOnlyCorrectOrders(listOfCorrectness, order)
    correctOrders.map(findMiddleValue).sum
  }

  private def part2(input: List[String]): Int = part2 {
    val rules             = prepInputRules(input)
    val order             = prepInputOrder(input)
    val rulesLkp          = prepareRulesLkp(rules)
    val permutations      = createAllPermutations(order)
    val listOfCorrectness = permutations.map(checkIfOrdersAreCorrect(_, rulesLkp))
    val onlyWrongOrders   = findOnlyWrongOrders(listOfCorrectness, order)
    onlyWrongOrders.map(x => fix(rules, x.toArray).toList).map(findMiddleValue).sum
  }
}
