package day11

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

import scala.annotation.tailrec

object Day11 extends Exercise(2024, 11) {

  def run(input: List[String]): Unit = {
    val testInput = List("125 17")
//    part1(input)
    part2(input)
  }

  private def prepInput(input: List[String]): List[Long] = {
    input.flatMap(_.split(" ")).map(_.toLong)
  }

  private def splitEvenStone(stone: String): List[String] = {
    val index       = stone.length / 2
    val firstStone  = stone.substring(0, index).toLong.toString
    val secondStone = stone.substring(index, stone.length).toLong.toString
    List(firstStone, secondStone)
  }

  private def blink(stones: List[Long]): List[Long] = {
    stones.flatMap {
      case x if x == 0 => List(1)
      case x if x.toString.length % 2 == 0 => splitEvenStone(x.toString).map(_.toLong)
      case x => List(x * 2024)
    }
  }

  @tailrec
  private def blinkNTimes(stones: List[Long], n: Int): List[Long] = {
    if (n <= 0) stones
    else {
      blinkNTimes(blink(stones), n - 1)
    }
  }

  private def part1(input: List[String]): Int = part1 {
    val preppedInput = prepInput(input)
    blinkNTimes(preppedInput, 25).length
  }

  private def part2(input: List[String]): Long = part2 {
    val preppedInput = prepInput(input)
    blinkNTimes(preppedInput, 75).length
  }
}
