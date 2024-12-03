package day03

import cats.effect.IO
import com.pg.bigdata.neighborhoodanalytics.aoc.fp.Exercise

import scala.annotation.tailrec

object Day03 extends Exercise(2024, 3) {

  private def prepInput(input: List[String]): String = input.mkString("")

  private def findMul(program: String): List[String] = {
    val pattern = """mul\(\d{1,3},\d{1,3}\)""".r
    pattern.findAllIn(program).toList
  }

  private def multiplyNumbers(singleMul: String): Int = {
    val pattern = """\d{1,3}""".r
    val numbers = pattern.findAllIn(singleMul).toList.map(_.toInt)
    numbers.product
  }

  private def findMulsAndDos(program: String): List[String] = {
    val pattern = """(mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\))""".r
    pattern.findAllIn(program).toList
  }

  private def removeMulsBetweenDontAndDo(input: List[String]) = {
    @tailrec
    def helper(remaining: List[String], acc: List[String], betweenDontAndDo: Boolean): List[String] = {
      remaining match
        case Nil                              => acc
        case "don't()" :: tail                => helper(tail, acc, true)
        case "do()" :: tail                   => helper(tail, acc, false)
        case head :: tail if betweenDontAndDo => helper(tail, acc, betweenDontAndDo)
        case head :: tail                     => helper(tail, acc :+ head, betweenDontAndDo)
    }
    helper(input, List.empty, false)
  }

  def part1(input: List[String]): IO[String] = {
    for {
      preppedInput <- IO.pure(prepInput(input))
      listOfMuls   <- IO.pure(findMul(preppedInput))
      result       <- IO.pure(listOfMuls.map(multiplyNumbers).sum)
    } yield result.toString
  }

  def part2(input: List[String]): IO[String] = {
    for {
      preppedInput     <- IO.pure(prepInput(input))
      listOfMulsAndDos <- IO.pure(findMulsAndDos(preppedInput))
      cleanList        <- IO.pure(removeMulsBetweenDontAndDo(listOfMulsAndDos))
      result           <- IO.pure(cleanList.map(multiplyNumbers).sum)
    } yield result.toString
  }
}
