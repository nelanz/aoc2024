package day04

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day04 extends Exercise(2024, 4) {

  def run(input: List[String]): Unit = {
    part1(input)
    part2(input)
  }

  private val pattern = """XMAS""".r

  // od lewej do prawej w linii horyzontalnej
  private def countLeftToRightHorizontal(line: String): Int = {
    pattern.findAllIn(line).toList.length
  }

  // od prawej do lewej w linii horyzontalnej
  private def countRightToLeftHorizontal(line: String): Int = {
    pattern.findAllIn(line.reverse).toList.length
  }

  // przygotowanie danych na wertykalne czytanie
  private def readVertically(input: List[String]): List[String] = {
    input.map(_.toList).transpose.map(_.mkString)
  }

  // od góry do dołu
  private def countTopToBottom(line: String): Int = {
    countLeftToRightHorizontal(line)
  }

  // od dołu do góry
  private def countBottomToTop(line: String): Int = {
    countRightToLeftHorizontal(line)
  }

  // przygotowanie danych na czytanie po skosie od lewej gory do prawego dolu albo od prawego dolu do lewej gory
  private def readDiagonallyTopLeftToBottomRight(input: List[String]): List[String] = {
    val numberOfCols = input.head.length
    val numberOfRows = input.length
    val diagonals    = ListBuffer[String]()

    for (k <- 0 until (numberOfRows + numberOfCols - 1)) { // wszystkie mozliwosci przekatnej
      val diagonal = new StringBuilder()
      for (i <- 0 until numberOfRows) {
        val j = k - i
        if (j >= 0 && j < numberOfCols) {
          diagonal.append(input(i)(j))
        }
      }
      diagonals += diagonal.toString()
    }
    diagonals.toList
  }

  private def readDiagonallyBottomLeftToTopRight(input: List[String]): List[String] = {
    val numberOfCols = input.head.length
    val numberOfRows = input.length
    val diagonals    = ListBuffer[String]()

    for (k <- 0 until (numberOfRows + numberOfCols - 1)) {
      val diagonal = new StringBuilder()
      for (i <- 0 until numberOfRows) {
        val j = k - (numberOfRows - 1 - i)
        if (j >= 0 && j < numberOfCols) {
          diagonal.append(input(i)(j))
        }
      }
      diagonals += diagonal.toString()
    }

    diagonals.toList
  }

  /*
  od lewej gory do prawego dołu na skos - jak stoje w a to przekatna skierowana w moja strone
  [a b c
  ,d e f
  ,g h i]
   List(a, bd, ceg, fh, i)
   */

  private def countLeftTopToRightBottom(line: String): Int = {
    countLeftToRightHorizontal(line)
  }

  // od lewej gory do prawego dołu na skos - czytanie w przeciwna strone
  // czyli od prawego gornego rogu do lewego gornego
  private def countLeftTopToRightBottomInverse(line: String): Int = {
    countRightToLeftHorizontal(line)
  }

  /*
  Od lewego dolu do prawej gory, jak stoje w g to przekatne skierowane w moja strone
  [a b c
  ,d e f
  ,g h i]
   List(g, dh, aei, bf, c)
   */
  private def countLeftBottomToRightTop(line: String): Int = {
    countLeftToRightHorizontal(line)
  }

  private def countLeftBottomToRightTopInverse(line: String): Int = {
    countRightToLeftHorizontal(line)
  }

  // part 2
  private def prepInputForList(input: List[String]): List[List[Char]] = {
    input.map(_.split("").flatMap(_.toCharArray).toList)
  }

  case class Counter(value: Int) {
    def increment: Counter = copy(value = value + 1)
  }

  private def countXMas(grid: List[List[Char]]) = {
    val numberOfRows = grid.head.length
    val numberOfCols = grid.length

    @tailrec
    def helper(row: Int, col: Int, counter: Int): Int = {
      if (row >= numberOfRows - 1) counter
      else if (col >= numberOfCols - 1) helper(row + 1, 1, counter) // przechodzimy o 1 dalej i od poczatku
      else {
        val currentCell = grid(row)(col)
        val newCounter: Int = if (currentCell == 'A') {
          val leftTopToRightBottom = (grid(row - 1)(col - 1), grid(row + 1)(col + 1))
          val leftBottomToRightTop = (grid(row + 1)(col - 1), grid(row - 1)(col + 1))

          if (checkMAS(leftTopToRightBottom._1, leftTopToRightBottom._2) && checkMAS(leftBottomToRightTop._1, leftBottomToRightTop._2)) {
            counter + 1
          } else counter
        } else counter
        helper(row, col + 1, newCounter)
      }
    }
    helper(1, 1, 0)
  }

  private def checkMAS(firstLetter: Char, lastLetter: Char): Boolean = {
    (firstLetter, lastLetter) match
      case ('M', 'S') => true
      case ('S', 'M') => true
      case _          => false
  }

  private def part1(input: List[String]): Int = part1 {
    val horizontalSum              = input.map(countLeftToRightHorizontal).sum + input.map(countRightToLeftHorizontal).sum
    val verticalLines              = readVertically(input)
    val verticalSum                = verticalLines.map(countTopToBottom).sum + verticalLines.map(countBottomToTop).sum
    val diagonalFromTopLeftToRight = readDiagonallyTopLeftToBottomRight(input)
    val diagonalFromTopLeftToRightSum =
      diagonalFromTopLeftToRight.map(countLeftTopToRightBottom).sum + diagonalFromTopLeftToRight.map(countLeftTopToRightBottomInverse).sum
    val diagonalFromBottomLeftToRight = readDiagonallyBottomLeftToTopRight(input)
    val diagonalFromBottomLeftToRightSum =
      diagonalFromBottomLeftToRight.map(countLeftBottomToRightTop).sum + diagonalFromBottomLeftToRight.map(countLeftBottomToRightTopInverse).sum

    horizontalSum + verticalSum + diagonalFromTopLeftToRightSum + diagonalFromBottomLeftToRightSum
  }

  private def part2(input: List[String]): Int = part2 {
    countXMas(prepInputForList(input))
  }

}
