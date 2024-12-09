package day09

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.postfixOps

object Day09 extends Exercise(2024, 9) {

  def run(input: List[String]): Unit = {
    val example  = List("12345")
    val example2 = List("2333133121414131402")
//    println(part1(input))
    println(part2(example2))
  }

  private def prepInput(input: List[String]): List[Int] = input.head.toList.map(_.toString.toInt)

  private def getBlocks(diskMap: List[Int]): List[Int] = {
    @tailrec
    def helper(acc: List[Int], idx: Int): List[Int] = {
      if (idx > diskMap.length - 1) acc
      else {
        if (idx == 0) {
          val repeatedList = List.fill(diskMap(idx))(0)
          helper(acc ++ repeatedList, idx + 1)
        } else if (idx % 2 == 0 && idx > 0) {
          val repeatedList = List.fill(diskMap(idx))(idx / 2)
          helper(acc ++ repeatedList, idx + 1)
        } else {
          val spacesList = List.fill(diskMap(idx))(-1)
          helper(acc ++ spacesList, idx + 1)
        }
      }
    }
    helper(List.empty, 0)
  }

  private def moveFileBlocks(fileBlocks: List[Int]): List[Int] = {
    @tailrec
    def helper(acc: List[Int], leftPointer: Int, rightPointer: Int): List[Int] = {
      if (leftPointer > rightPointer) acc
      else {
        if (fileBlocks(leftPointer) != -1) helper(acc :+ fileBlocks(leftPointer), leftPointer + 1, rightPointer)
        else if (fileBlocks(rightPointer) == -1) helper(acc, leftPointer, rightPointer - 1)
        else {
          helper(acc :+ fileBlocks(rightPointer), leftPointer + 1, rightPointer - 1)
        }
      }
    }

    helper(List.empty, 0, fileBlocks.length - 1)
  }

  private def calculateCheckSum(movedFileBlocks: List[Int]): Long = {
    @tailrec
    def helper(sum: Long, idx: Int): Long = {
      if (idx > movedFileBlocks.length - 1) sum
      else {
        val singleSum = idx * movedFileBlocks(idx)
        helper(sum + singleSum, idx + 1)
      }
    }
    helper(0L, 0)
  }

  // PART 2 - Not working

  trait Block {
    def length: Int
  }
  case class SpaceBlock(length: Int)         extends Block
  case class FileBlock(id: Int, length: Int) extends Block

  private def getBlockObjects(diskMap: List[Int]): List[Block] = {
    @tailrec
    def helper(acc: List[Block], idx: Int): List[Block] = {
      if (idx > diskMap.length - 1) acc
      else {
        if (idx % 2 == 0) helper(acc :+ FileBlock(idx / 2, diskMap(idx)), idx + 1)
        else helper(acc :+ SpaceBlock(diskMap(idx)), idx + 1)
      }
    }
    helper(List.empty, 0).filter(_.length > 0)
  }

  private def unifySpaceBlocks(blockObjectsBuffer: ArrayBuffer[Block]): ArrayBuffer[Block] = {
    blockObjectsBuffer.foldRight(ArrayBuffer.empty[Block]) {
      case (SpaceBlock(length), SpaceBlock(accLength) +: tail) => SpaceBlock(length + accLength) +: tail
      case (block, acc)                                        => block +: acc
    }
  }

  private def moveBlockObjects(blockObjects: List[Block]): List[Block] = {
    @tailrec
    def helper(idx: Int, blockObjectsBuffer: ArrayBuffer[Block]): List[Block] = {
      if (idx < 1) blockObjectsBuffer.toList
      else {
        val endObject = blockObjects(idx)
        println(endObject)
        println(blockObjectsBuffer)
        println(s"Current indx $idx")
        endObject match
          case SpaceBlock(n) => helper(idx - 1, blockObjectsBuffer)
          case FileBlock(id, n) => {
            val firstPlaceToMove = blockObjectsBuffer.indexWhere {
              case SpaceBlock(length) if length >= n => true
              case _                                 => false
            }
            println(s"First place to move $firstPlaceToMove")
            if (id == 1) blockObjectsBuffer.toList
            else if (firstPlaceToMove != -1) {
              val removed = blockObjectsBuffer.remove(firstPlaceToMove)
              blockObjectsBuffer.insertAll(firstPlaceToMove, Seq(FileBlock(id, n), SpaceBlock(removed.length - n)))
              blockObjectsBuffer.remove(idx + 1)
              println(s"REMOVED ${blockObjectsBuffer}")
              blockObjectsBuffer.insertAll(idx + 1, Seq(SpaceBlock(n)))
              println(s"INSERTED $blockObjectsBuffer")
              val unifiedBuffer = unifySpaceBlocks(blockObjectsBuffer)
              helper(idx - 1, unifiedBuffer)
            } else {
              helper(idx - 1, blockObjectsBuffer)
            }
          }
      }
    }
    helper(blockObjects.length - 1, ArrayBuffer(blockObjects: _*))
  }

  private def part1(input: List[String]) = {
    val preppedInput    = prepInput(input)
    val fileBlocks      = getBlocks(preppedInput)
    val movedFileBlocks = moveFileBlocks(fileBlocks)
    calculateCheckSum(movedFileBlocks)
  }

  private def part2(input: List[String]) = {
    val preppedInput = prepInput(input)
    val fileBlocks   = getBlockObjects(preppedInput)
    println(fileBlocks)
    moveBlockObjects(fileBlocks)
  }
}
