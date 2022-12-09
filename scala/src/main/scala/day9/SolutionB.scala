package day9

import java.nio.file.{Files, Paths}
import scala.collection.immutable.Stream.Empty

object SolutionB extends App {

  sealed trait Direction

  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Move(direction: Direction, steps: Int)

  val input = Files.readString(Paths.get("src/main/resources/day9/input_a.txt"))

  //  val input =
  //    """R 4
  //      |U 4
  //      |L 3
  //      |D 1
  //      |R 4
  //      |D 1
  //      |L 5
  //      |R 2""".stripMargin

  val moves = input
    .split('\n')
    .map(_.split(' '))
    .map {
      case Array(directionString, steps) =>
        val direction = directionString match {
          case "R" => Right
          case "L" => Left
          case "U" => Up
          case "D" => Down
        }
        Move(direction, steps.toInt)
    }
    .toList

  case class Position(x: Int, y: Int) {
    def touching(other: Position): Boolean = {
      val candidates = for {
        xx <- x - 1 to x + 1
        yy <- y - 1 to y + 1
      } yield Position(xx, yy)

      candidates.contains(other)
    }
  }

  def createHeadPositions(moves: List[Move], start: Position): Stream[Position] = moves match {
    case Nil => Stream.empty
    case Move(direction, amount) :: tail =>
      val positions = direction match {
        case Up => Stream.from(start.y + 1).take(amount).map(y => Position(start.x, y))
        case Down => Stream.from(start.y - 1, -1).take(amount).map(y => Position(start.x, y))
        case Left => Stream.from(start.x - 1, -1).take(amount).map(x => Position(x, start.y))
        case Right => Stream.from(start.x + 1).take(amount).map(x => Position(x, start.y))
      }

      positions #::: createHeadPositions(tail, positions.last)
  }

  def createKnotPositions(headPositions: Stream[Position], start: Position): Stream[Position] = headPositions match {
    case Empty => Stream.empty
    case head #:: remaining =>
      if (start.touching(head)) start #:: createKnotPositions(remaining, start)
      else {
        val xOffset = math.signum(head.x - start.x)
        val yOffset = math.signum(head.y - start.y)

        val position = Position(start.x + xOffset, start.y + yOffset)
        position #:: createKnotPositions(remaining, position)
      }
  }

  val headPositions = createHeadPositions(moves, Position(x = 0, y = 0))

  val tailPositions = (1 to 9).foldLeft(headPositions) { case (acc, _) =>
    createKnotPositions(acc, Position(x = 0, y = 0))
  }

  val result = tailPositions.toSet.size

  println(result) // 2651
}
