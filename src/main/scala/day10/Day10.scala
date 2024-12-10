package day10

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

import scala.annotation.tailrec

object Day10 extends Exercise(2024, 10) {

  def run(input: List[String]): Unit = {
    part1(input)
    part2(input)
  }

  private def prepInput(input: List[String]): List[List[Int]] = {
    input.map(_.toList.map(_.toString.toInt))
  }

  trait Direction {
    val row: Int
    val col: Int
  }
  private case object Up extends Direction {
    val row: Int = -1
    val col: Int = 0
  }
  private case object Down extends Direction {
    val row: Int = 1
    val col: Int = 0
  }
  private case object Left extends Direction {
    val row: Int = 0
    val col: Int = -1
  }
  private case object Right extends Direction {
    val row: Int = 0
    val col: Int = 1
  }

  private val directions: List[Direction] = List(Up, Down, Left, Right)

  private def dfs(input: List[List[Int]], x: Int, y: Int, visited: Array[Array[Boolean]]): List[(Int, Int)] = {
    visited(x)(y) = true

    val currentResult = if (input(x)(y) == 9) List((x, y)) else List.empty

    val adjacentResults = directions.flatMap { case direction: Direction =>
      val moveRow = direction.row
      val moveCol = direction.col
      val newRow  = x + moveRow
      val newCol  = y + moveCol

      if (
        newRow >= 0 && newRow < input.length
        && newCol >= 0 && newCol < input.head.length
        && !visited(newRow)(newCol)
        && input(newRow)(newCol) == input(x)(y) + 1
      ) {
        dfs(input, newRow, newCol, visited)
      } else {
        List.empty
      }
    }

    visited(x)(y) = false
    currentResult ++ adjacentResults
  }

  private def findNines(input: List[List[Int]]): List[List[(Int, Int)]] = {
    @tailrec
    def helper(acc: List[List[(Int, Int)]], visited: Array[Array[Boolean]], row: Int, col: Int): List[List[(Int, Int)]] = {
      if (row >= input.length) acc
        
      else if (col >= input.head.length) helper(acc, visited, row + 1, 0)
        
      else if (input(row)(col) == 0) {
        val result = dfs(input, row, col, visited)
        helper(acc :+ result, visited, row, col + 1)
        
      } else {
        helper(acc, visited, row, col + 1)
      }
    }
    val visited = Array.fill(input.length, input.head.length)(false)
    helper(List.empty, visited, 0, 0)
  }

  private def findScores(nines: List[List[(Int, Int)]]): List[Int] = {
    nines.map(_.distinct.length)
  }

  private def part1(input: List[String]): Int = part1 {
    val preppedInput = prepInput(input)
    val results      = findNines(preppedInput)
    findScores(results).sum
  }

  private def part2(input: List[String]): Int = part2 {
    val preppedInput = prepInput(input)
    val results      = findNines(preppedInput)
    results.map(_.length).sum
  }
}
