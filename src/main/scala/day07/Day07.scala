package day07

import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import cats.Monad
import cats.implicits._

object Day07 extends Exercise(2024, 7) {

  def run(input: List[String]): Unit = {
    println(part1(input))
    println(part2(input))
  }

  private case class Equation(result: Long, numbers: List[Long]) {

    def createTree: Tree[Long] = {
      val startTree =
        TripleBranch(Leaf(numbers.head + numbers(1)), Leaf(numbers.head * numbers(1)), Leaf(numbers.head.toString.concat(numbers(1).toString).toLong))
      if (numbers.length == 2) startTree
      else {
        numbers.drop(2).foldLeft(startTree: Tree[Long]) { (tree: Tree[Long], num: Long) =>
          TreeMonad.flatMap(tree)(v => TripleBranch(Leaf(v * num), Leaf(v + num), Leaf(v.toString.concat(num.toString).toLong)))
        }
      }
    }

    def getLeafs: List[Long] =
      val tree = this.createTree
      TreeMonad.extractLeaves(tree)

    def getResultsThatAreCorrect: Long =
      val listOfLeafs = this.getLeafs
      if (listOfLeafs.contains(this.result)) result else 0
  }

  private def prepInput(input: List[String]): List[Equation] = {
    input.map { x =>
      val result  = x.split(":").head.toLong
      val numbers = x.split(":")(1).trim.split(" ").toList.map(_.toLong)
      Equation(result, numbers)
    }
  }


  sealed trait Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class TripleBranch[A](left: Tree[A], middle: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {

    def pure[A](x: A): Tree[A] = Leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v)                  => f(v)
      case Branch(lt, rt)           => Branch(flatMap(lt)(f), flatMap(rt)(f))
      case TripleBranch(lt, mt, rt) => TripleBranch(flatMap(lt)(f), flatMap(mt)(f), flatMap(rt)(f))
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???

    def extractLeaves[A](tree: Tree[A]): List[A] = {
      tree match
        case Leaf(v) => List(v)
        case Branch(left, right) =>
          extractLeaves(left) ++ extractLeaves(right)
        case TripleBranch(left, middle, right) => extractLeaves(left) ++ extractLeaves(middle) ++ extractLeaves(right)
    }
  }

  private def part1(input: List[String]) = {
    val equations = prepInput(input)
    val leafs     = equations.map(_.getLeafs)
    equations.map(_.getResultsThatAreCorrect).sum
  }

  private def part2(input: List[String]) = {
    val equations = prepInput(input)
    val leafs     = equations.map(_.getLeafs)
    equations.map(_.getResultsThatAreCorrect).sum
  }
}
