package day14

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import java.lang.System.lineSeparator

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day14/input_a.txt"))

//  val input =
//    """498,4 -> 498,6 -> 496,6
//      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  sealed trait Tile
  case object Air extends Tile
  case object Rock extends Tile
  case object Sand extends Tile

  case class Coordinates(x: Int, y: Int)


  class Grid(values: Array[Array[Tile]], xOffset: Int, yOffset: Int) {

    def height: Int = values.length

    def width: Int = values.headOption.map(_.length).getOrElse(0)

    def topLeft: Coordinates = Coordinates(xOffset, yOffset)

    def bottomRight: Coordinates = Coordinates(xOffset + width - 1, yOffset + height - 1)

    def isBlocked(x: Int, y: Int): Boolean = values(y - yOffset)(x - xOffset) != Air

    def update(x: Int, y: Int, tile: Tile): Grid = {

      // TODO make immutable
      tiles(y - yOffset)(x - xOffset) = tile
      this

    }

    def countSand(): Int = values.map(_.count(_ == Sand)).sum

    override def toString: String = {
      values
        .map { row =>
          row.map {
            case Rock => '#'
            case Air => '.'
            case Sand => 'o'
          }.mkString
        }.mkString(lineSeparator)
    }
  }

  val rockRanges = input
    .split('\n')
    .map(_
      .split(" -> ")
      .map(_.split(","))
      .toList
      .map { case Array(x, y) => Coordinates(x.toInt, y.toInt) }
    )

  val start = Coordinates(x = 500, y = 0)


  val rockCoordinates = rockRanges
    .flatMap(_
      .sliding(2)
      .flatMap {
        case List(Coordinates(fromX, fromY), Coordinates(toX, toY))  =>
          if (fromX == toX) Range.inclusive(fromY, toY, math.signum(toY - fromY))
                              .map(y => Coordinates(fromX, y))
          else Range.inclusive(fromX, toX, math.signum(toX - fromX))
                 .map(x => Coordinates(x, fromY))
      }
    )
    .toSet

  val all = start :: rockRanges.flatten.toList

  val minX = all.map(_.x).min
  val maxX = all.map(_.x).max

  val minY = all.map(_.y).min
  val maxY = all.map(_.y).max

  val xPadding = 1

  val tiles = Range.inclusive(minY, maxY + 1).map { y =>
    Range.inclusive(minX - xPadding, maxX + xPadding).map { x =>
      if (rockCoordinates.contains(Coordinates(x, y))) Rock
      else Air
    }.toArray[Tile]
  }.toArray

  val grid = new Grid(tiles, minX - xPadding, minY)

  @tailrec def findRestingPoint(start: Coordinates, grid: Grid): Coordinates = {
    List(
      Coordinates(start.x, start.y + 1),
      Coordinates(start.x - 1, start.y + 1),
      Coordinates(start.x + 1, start.y + 1)
    )
    .filter { coordinates => coordinates.y <= grid.bottomRight.y }
    .find { case Coordinates(x, y) => !grid.isBlocked(x, y) } match {
      case Some(coordinates) => findRestingPoint(coordinates, grid)
      case _ => start
    }
  }

  @tailrec def update(grid: Grid): Grid = {
    val restingCoords = findRestingPoint(start, grid)

    if (restingCoords.y == grid.bottomRight.y) grid
    else {
      val newGrid = grid.update(restingCoords.x, restingCoords.y, Sand)
      update(newGrid)
    }
  }

  val finalGrid = update(grid)

  println(finalGrid.countSand()) // 719


}

