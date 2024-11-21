package example

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import scala.annotation.tailrec

object Day1 extends Exercise(2021, 1) {
  def run(input: List[String]): Unit = {
    val inputData = prepInput(input)
    println(inputData)
    part1(inputData)
    part2(inputData)
    ()
  }

  private def prepInput(input: List[String]): List[Int] = input.map(_.toInt)

  private def countDepth(measurements: List[Int]): Int = {
    @tailrec
    def helper(previousVal: Int, acc: Int, measurements: List[Int]): Int = {
      if (measurements.isEmpty) acc
      else if (measurements.head > previousVal) helper(measurements.head, acc + 1, measurements.tail)
      else helper(measurements.head, acc, measurements.tail)
    }

    helper(measurements.head, 0, measurements.tail)
  }

  private def createWindows(measurements: List[Int]): List[Int] = {
    @tailrec
    def helper(firstVal: Int, secondVal: Int, thirdVal: Int, acc: List[Int], measurements: List[Int]): List[Int] = {
      if (measurements.isEmpty) acc :+ (firstVal + secondVal + thirdVal)
      else helper(secondVal, thirdVal, measurements.head, acc :+ (firstVal + secondVal + thirdVal), measurements.tail)
    }

    Thread.sleep(2000)
    helper(measurements.head, measurements.tail.head, measurements.tail.tail.head, List.empty, measurements.tail.tail.tail)
  }

  private def part1(input: List[Int]): Int = part1 {
    countDepth(input)
  }

  private def part2(input: List[Int]): Int = part2 {
    countDepth(createWindows(input))
  }
}
