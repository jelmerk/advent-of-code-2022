package day5

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.Stack

object SolutionB extends App {

  case class Command(amount: Int, from: Int, to: Int)

  val input = Files.readString(Paths.get("src/main/resources/day5/input_a.txt"))


//  val input =
//    """    [D]
//      |[N] [C]
//      |[Z] [M] [P]
//      | 1   2   3
//      |
//      |move 1 from 2 to 1
//      |move 3 from 1 to 3
//      |move 2 from 2 to 1
//      |move 1 from 1 to 2""".stripMargin


  val Array(state, instructions) = input.split("\n\n")

  val stateLines = state.split("\n")

  val maxWidth = stateLines.map(_.length).max

  val stacks = stateLines
    .map { _.padTo(maxWidth, ' ').grouped(4).toList }
    .toList
    .transpose
    .map { row =>
      val index = row.last
      val values = row.dropRight(1)

      index.trim.toInt -> values.map(_.trim).filterNot(_.isEmpty).map(_.drop(1).dropRight(1))
    }
    .toMap

  val CommandString = """move (\d+) from (\d+) to (\d+)""".r

  val commands = instructions
    .split('\n')
    .map { case CommandString(amount, from, to) => Command(amount.toInt, from.toInt, to.toInt) }


//  commands.foreach(println)

  val newState = commands
    .foldLeft(stacks) {
      case (acc, Command(amount, from, to)) =>

        val fromStack = acc(from)
        val toStack = acc(to)

        val (taken, newFromStack) = fromStack.splitAt(amount)
        val newToStack = taken ::: toStack

        val updated = acc
          .updated(from, newFromStack)
          .updated(to, newToStack)

        updated

    }

  val result = newState.toList.sortBy(_._1).map { case (_, stack) => stack.head }.mkString("")
  println(result) // JNRSCDWPP

}
