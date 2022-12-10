package day10

import java.nio.file.{Files, Paths}
import java.lang.System.lineSeparator

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day10/input_a.txt"))

//  val input =
//    """addx 15
//      |addx -11
//      |addx 6
//      |addx -3
//      |addx 5
//      |addx -1
//      |addx -8
//      |addx 13
//      |addx 4
//      |noop
//      |addx -1
//      |addx 5
//      |addx -1
//      |addx 5
//      |addx -1
//      |addx 5
//      |addx -1
//      |addx 5
//      |addx -1
//      |addx -35
//      |addx 1
//      |addx 24
//      |addx -19
//      |addx 1
//      |addx 16
//      |addx -11
//      |noop
//      |noop
//      |addx 21
//      |addx -15
//      |noop
//      |noop
//      |addx -3
//      |addx 9
//      |addx 1
//      |addx -3
//      |addx 8
//      |addx 1
//      |addx 5
//      |noop
//      |noop
//      |noop
//      |noop
//      |noop
//      |addx -36
//      |noop
//      |addx 1
//      |addx 7
//      |noop
//      |noop
//      |noop
//      |addx 2
//      |addx 6
//      |noop
//      |noop
//      |noop
//      |noop
//      |noop
//      |addx 1
//      |noop
//      |noop
//      |addx 7
//      |addx 1
//      |noop
//      |addx -13
//      |addx 13
//      |addx 7
//      |noop
//      |addx 1
//      |addx -33
//      |noop
//      |noop
//      |noop
//      |addx 2
//      |noop
//      |noop
//      |noop
//      |addx 8
//      |noop
//      |addx -1
//      |addx 2
//      |addx 1
//      |noop
//      |addx 17
//      |addx -9
//      |addx 1
//      |addx 1
//      |addx -3
//      |addx 11
//      |noop
//      |noop
//      |addx 1
//      |noop
//      |addx 1
//      |noop
//      |noop
//      |addx -13
//      |addx -19
//      |addx 1
//      |addx 3
//      |addx 26
//      |addx -30
//      |addx 12
//      |addx -1
//      |addx 3
//      |addx 1
//      |noop
//      |noop
//      |noop
//      |addx -9
//      |addx 18
//      |addx 1
//      |addx 2
//      |noop
//      |noop
//      |addx 9
//      |noop
//      |noop
//      |noop
//      |addx -1
//      |addx 2
//      |addx -37
//      |addx 1
//      |addx 3
//      |noop
//      |addx 15
//      |addx -21
//      |addx 22
//      |addx -6
//      |addx 1
//      |noop
//      |addx 2
//      |addx 1
//      |noop
//      |addx -10
//      |noop
//      |noop
//      |addx 20
//      |addx 1
//      |addx 2
//      |addx 2
//      |addx -6
//      |addx -11
//      |noop
//      |noop
//      |noop""".stripMargin

  case class State(x: Int, runtime: Long)

  sealed trait Command

  case class AddX(value: Int) extends Command

  case object Noop extends Command

  val AddXString = "addx (.*)".r
  val NoopString = "noop"

  val commands = input
    .split(lineSeparator)
    .map {
      case AddXString(value) => AddX(value.toInt)
      case NoopString => Noop
    }
    .toList

  def execute(state: State, commands: List[Command]): Stream[State] = commands match {
    case Nil => state #:: Stream.empty
    case command :: tail =>
      val newState = command match {
        case AddX(value) => state.copy(x = state.x + value, runtime = state.runtime + 2)
        case Noop => state.copy(runtime = state.runtime + 1)
      }
      state #:: execute(newState, tail)
  }

  val initialState = State(x = 1, runtime = 0)
  val states = execute(initialState, commands)

  val result = List(20, 60, 100, 140, 180, 220)
    .flatMap { cycle =>
      states.takeWhile(_.runtime < cycle).lastOption.map(_.x * cycle)
    }
    .sum

  println(result) // 12520

}
