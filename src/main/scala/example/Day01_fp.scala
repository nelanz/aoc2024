package example

import cats.effect.IO
import com.pg.bigdata.neighborhoodanalytics.aoc.fp.Exercise

import scala.annotation.tailrec

object Day01_fp extends Exercise(2021, 1) {

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

  def part1(input: List[String]): IO[String] = {
    for {
      prepped <- IO.pure(prepInput(input))
      depth   <- IO.pure(countDepth(prepped))
    } yield depth.toString
  }

  def part2(input: List[String]): IO[String] = {
    for {
      prepped  <- IO.pure(prepInput(input))
      windowed <- IO.pure(createWindows(prepped))
      depth    <- IO.pure(countDepth(windowed))
    } yield depth.toString
  }
}