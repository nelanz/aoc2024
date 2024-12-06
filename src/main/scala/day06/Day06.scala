package day06

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

import scala.annotation.tailrec

object Day06 extends Exercise(2024, 6) {

  def run(input: List[String]): Unit = {
//    val testInput: List[String] = List(
//      "....#.....",
//      ".........#",
//      "..........",
//      "..#.......",
//      ".......#..",
//      "..........",
//      ".#..^.....",
//      "........#.",
//      "#.........",
//      "......#..."
//    )
//    println(testInput)
    part1(input)
  }

  private def prepInput(input: List[String]): List[List[Char]] = {
    input.map(_.toList) // to samo co split i potem flatMap toCharArray
  }

  private def turn(direction: Char): Char = {
    direction match
      case '^' => '>'
      case '>' => 'v'
      case 'v' => '<'
      case '<' => '^'
  }

  private def walkForward(direction: Char, currPosition: (Int, Int)): (Int, Int) = {
    if (direction == '^') (currPosition._1 - 1, currPosition._2)
    else if (direction == '>') (currPosition._1, currPosition._2 + 1)
    else if (direction == '<') (currPosition._1, currPosition._2 - 1)
    else (currPosition._1 + 1, currPosition._2)
  }

  private def doWeLeave(row: Int, col: Int, numberOfRows: Int, numberOfCols: Int, direction: Char, obstruction: Char): Boolean = {
    if ((row >= numberOfRows - 1) && direction == 'v' && obstruction == '.') true
    else if ((row <= 0) && direction == '^' && obstruction == '.') true
    else if ((col >= numberOfCols - 1) && direction == '>' && obstruction == '.') true
    else if ((col <= 0) && direction == '<' && obstruction == '.') true
    else false
  }

  private def moveOnGrid(grid: List[List[Char]]) = {
    val startingRow  = grid.map(_.contains('^')).indexOf(true)
    val startingCol  = grid(startingRow).indexOf('^')
    val numberOfRows = grid.length
    val numberOfCols = grid.head.length

    @tailrec
    def helper(currDirection: Char, currRow: Int, currCol: Int, visited: List[(Int, Int)]): List[(Int, Int)] = {
      val currObstruction = grid(currRow)(currCol)
      if (doWeLeave(currRow, currCol, numberOfRows, numberOfCols, currDirection, currObstruction)) visited :+ (currRow, currCol)
      else {
        if (currObstruction == '#') {
          val newDirection = turn(currDirection)
          helper(newDirection, visited.last._1, visited.last._2, visited)
        } else {
          val walk = walkForward(currDirection, (currRow, currCol))
          helper(currDirection, walk._1, walk._2, visited :+ (currRow, currCol))
        }
      }
    }
    helper('^', startingRow, startingCol, List.empty)
  }

  private def part1(input: List[String]): Int = part1 {
    val grid        = prepInput(input)
    val startingRow = grid.map(_.contains('^')).indexOf(true)
    val startingCol = grid(startingRow).indexOf('^')
    moveOnGrid(grid).toSet.size
  }
}
