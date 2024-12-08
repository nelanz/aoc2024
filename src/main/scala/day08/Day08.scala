package day08

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

import scala.annotation.tailrec

object Day08 extends Exercise(2024, 8) {

  def run(input: List[String]): Unit = {
    part1(input)
    part2(input)
  }

  private def prepInput(input: List[String]): List[List[Char]] = {
    input.map(_.toList)
  }

  private case class Antenna(freq: Char, position: (Int, Int))

  private def findAntennas(lists: List[List[Char]]): Map[Char, List[(Int, Int)]] = {
    val positions = for {
      (row, rowIndex)  <- lists.zipWithIndex
      (char, colIndex) <- row.zipWithIndex
      if char != '.'
    } yield (char, (rowIndex, colIndex))

    positions.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
  }

  private def findAntennaGroups(antennas: Map[Char, List[(Int, Int)]]): Map[Char, List[(Int, Int)]] = {
    antennas.filter(_._2.length > 1)
  }

  private def generatePairs(points: List[(Int, Int)]): List[List[(Int, Int)]] = {
    for {
      (point1, index1) <- points.zipWithIndex
      (point2, index2) <- points.zipWithIndex
      if index1 < index2
    } yield List(point1, point2)
  }

  private def findDifference(pair: List[(Int, Int)]): (Int, Int) = {
    val first  = pair.head
    val second = pair.last
    (first._1 - second._1, first._2 - second._2)
  }

  private def adjustPairs(pairs: List[List[(Int, Int)]], differences: List[(Int, Int)]): List[List[(Int, Int)]] = {
    pairs.zip(differences).map { case (pair, diff) =>
      val adjustedFirst  = (pair.head._1 + diff._1, pair.head._2 + diff._2)
      val adjustedSecond = (pair.last._1 - diff._1, pair.last._2 - diff._2)
      List(adjustedFirst, adjustedSecond)
    }
  }

  private def adjustPairsUntilValid(pairs: List[List[(Int, Int)]], differences: List[(Int, Int)], numberOfRows: Int, numberOfCols: Int): List[(Int, Int)] = {
    def isValid(pair: (Int, Int)): Boolean = {
      pair._1 < numberOfRows && pair._2 < numberOfCols && pair._1 >= 0 && pair._2 >= 0
    }

    @tailrec
    def collectValidPoints(pair: (Int, Int), diff: (Int, Int), acc: Set[(Int, Int)] = Set()): Set[(Int, Int)] = {
      if (isValid(pair)) {
        collectValidPoints((pair._1 + diff._1, pair._2 + diff._2), diff, acc + pair)
      } else {
        acc
      }
    }

    pairs
      .zip(differences)
      .flatMap { case (pair, diff) =>
        val first  = pair.head
        val second = pair.last
        collectValidPoints(first, diff) ++ collectValidPoints(second, (-diff._1, -diff._2))
      }
      .distinct
  }

  private def findAntinodes(groupedAntennas: Map[Char, List[(Int, Int)]], numberOfRows: Int, numberOfCols: Int) = {
    groupedAntennas.map { x =>
      val points               = x._2
      val pairs                = generatePairs(points)
      val differences          = pairs.map(findDifference)
      val pairsWithDifferences = pairs.zip(differences)
      val adjustedPairs        = adjustPairs(pairs, differences)
      adjustedPairs.flatten.filter((a, b) => a < numberOfRows && b < numberOfCols && a >= 0 && b >= 0)
    }
  }

  private def findAllAntinodes(groupedAntennas: Map[Char, List[(Int, Int)]], numberOfRows: Int, numberOfCols: Int) = {
    groupedAntennas.map { x =>
      val points               = x._2
      val pairs                = generatePairs(points)
      val differences          = pairs.map(findDifference)
      val pairsWithDifferences = pairs.zip(differences)
      adjustPairsUntilValid(pairs, differences, numberOfRows, numberOfCols)
    }
  }

  private def part1(input: List[String]): Int = part1 {
    val preppedInput       = prepInput(input)
    val antennas           = findAntennas(preppedInput)
    val antennaGroup       = findAntennaGroups(antennas)
    val antinodesLocations = findAntinodes(antennaGroup, preppedInput.length, preppedInput.head.length)
    antinodesLocations.flatten.toSet.size
  }

  private def part2(input: List[String]): Int = part2 {
    val preppedInput       = prepInput(input)
    val antennas           = findAntennas(preppedInput)
    val antennaGroup       = findAntennaGroups(antennas)
    val antinodesLocations = findAllAntinodes(antennaGroup, preppedInput.length, preppedInput.head.length)
    antinodesLocations.flatten.toSet.size
  }
}
