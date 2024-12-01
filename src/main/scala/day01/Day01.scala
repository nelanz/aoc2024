package day01

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

object Day01 extends Exercise(2024, 1) {
  def run(input: List[String]): Unit = {
    val inputData = prepInput(input)
    part1(inputData)
    part2(inputData)
  }

  private def prepInput(input: List[String]): List[List[Int]] = {
    List(input.map(_.split(" ").head.toInt), input.map(_.split(" ").last.toInt))
  }

  private def subtractLists(firstList: List[Int], secondList: List[Int]): List[Int] = {
    firstList.zip(secondList).map { case (a, b) => math.abs(a - b) }
  }

  private def countDistance(inputData: List[List[Int]]): Int = {
    val sortedLists = inputData.map(_.sorted)
    val distances   = subtractLists(sortedLists.head, sortedLists.last)
    distances.sum
  }

  private def findCount(el: Int, list: List[Int]): Int = {
    list.count(_ == el)
  }

  private def findSimilarityScore(inputData: List[List[Int]]): Int = {
    val firstList  = inputData.head
    val secondList = inputData.last

    firstList.map(x => x * findCount(x, secondList)).sum
  }

  private def part1(input: List[List[Int]]): Int = part1 {
    countDistance(input)
  }

  private def part2(input: List[List[Int]]): Int = part2 {
    findSimilarityScore(input)
  }
}
