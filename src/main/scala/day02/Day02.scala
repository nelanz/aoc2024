package day02

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import day02.Day02.Direction.{Decrease, Increase}
import day02.Day02.ReportSafety.Safe
import enumeratum.*

object Day02 extends Exercise(2024, 2) {

  def run(input: List[String]): Unit = {
    val dataInput = prepInput(input)
    part1(dataInput)
    part2(dataInput)
  }

  private def prepInput(input: List[String]): List[List[Int]] = {
    input.map(_.split(" ").map(_.toInt).toList)
  }

  private val WINDOW_SIZE = 2

  sealed trait ReportSafety extends EnumEntry

  object ReportSafety extends Enum[ReportSafety] {
    case object Safe extends ReportSafety

    case object Unsafe extends ReportSafety

    val values: IndexedSeq[ReportSafety] = findValues
  }

  sealed trait Direction extends EnumEntry

  object Direction extends Enum[Direction] {
    case object Increase extends Direction

    case object Decrease extends Direction

    val values: IndexedSeq[Direction] = findValues
  }

  private case class Report(reportInput: List[Int]) {
    private def splitIntoTuples(list: List[Int]): List[(Int, Int)] = {
      list.sliding(WINDOW_SIZE).collect { case Seq(a, b) => (a, b) }.toList
    }

    private def calculateDirection(tuple: (Int, Int)): Direction = {
      if (tuple._1 > tuple._2) Decrease
      else Increase
    }

    def getReportSafety: ReportSafety = {
      val tuples     = splitIntoTuples(reportInput)
      val directions = tuples.map(calculateDirection)
      val diffs      = tuples.map((a, b) => math.abs(a - b))

      if (directions.contains(Increase) && directions.contains(Decrease)) ReportSafety.Unsafe
      else if (diffs.exists(_ > 3) || diffs.exists(_ < 1)) ReportSafety.Unsafe
      else ReportSafety.Safe
    }

    def getReportSafetyWithDampener: ReportSafety = {
      if (
        reportInput.indices.exists { ind =>
          val dropped = reportInput.take(ind) ++ reportInput.drop(ind + 1)
          Report(dropped).getReportSafety == ReportSafety.Safe
        }
      ) ReportSafety.Safe
      else ReportSafety.Unsafe
    }
  }

  private def part1(input: List[List[Int]]): Int = part1 {
    input.map(Report(_).getReportSafety).count(_ == Safe)
  }

  private def part2(input: List[List[Int]]): Int = part2 {
    input.map(Report(_).getReportSafetyWithDampener).count(_ == Safe)
  }
}
