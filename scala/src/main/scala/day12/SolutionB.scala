package day12

import util.dijkstra.{DijkstraImmutable, SimpleGraph}

import java.nio.file.{Files, Paths}

object SolutionB extends App {

  val input = Files.readString(Paths.get("src/main/resources/day12/input_a.txt"))

  //  val input =
  //    """Sabqponm
  //      |abcryxxl
  //      |accszExk
  //      |acctuvwj
  //      |abdefghi""".stripMargin

  case class Position(x: Int, y: Int)

  class Grid[T] (values: Array[Array[T]]) {

    def height: Int = values.length

    def width: Int = values.headOption.map(_.length).getOrElse(0)

    def valueAt(x: Int, y: Int): T = values(y)(x)

  }

  case class Node(id: Int, connections: Set[Int])

  def toId[T](x: Int, y: Int, grid: Grid[T]): Int = y * grid.width + x

  val asCharacters: Array[Array[Char]] = input
    .split('\n')
    .map(_.toArray)

  val characterGrid = new Grid(asCharacters)

  val positions = for {
    y <- 0 until characterGrid.height
    x <- 0 until characterGrid.width
  } yield Position(x, y)

  val start = positions
    .find { case Position(x, y) =>
      characterGrid.valueAt(x, y) == 'S'
    }
    .getOrElse(throw new IllegalArgumentException("No start found."))

  val end = positions
    .find { case Position(x, y) =>
      characterGrid.valueAt(x, y) == 'E'
    }
    .getOrElse(throw new IllegalArgumentException("No end found."))

  val asValues = asCharacters.map {
    _.map {
      case 'S' => 1
      case 'E' => 26
      case character => character.toInt - 96
    }
  }

  val grid = new Grid(asValues)

  val nodes = (
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
    } yield {
      val connections = for {
        (xx, yy) <- Set(
          (x, y + 1),
          (x, y - 1),
          (x - 1, y),
          (x + 1, y)
        )
        if xx >= 0 && yy >= 0 && xx < grid.width && yy < grid.height
        if grid.valueAt(xx, yy) <= grid.valueAt(x, y) + 1
      } yield toId(xx, yy, grid)

      val id = toId(x, y, grid)
      val weight = 1
      id -> connections.map(connectedId => connectedId -> weight).toMap
    }).toMap

  val candidates =
    for {
      y <- 0 until grid.height
      x <- 0 until grid.width
      if grid.valueAt(x, y) == 1
    } yield toId(x, y, grid)

  val graph = SimpleGraph(nodes)

  val endNode = toId(end.x, end.y, grid)

  val result = candidates
    .flatMap { startNode => DijkstraImmutable.shortestPath(graph)(startNode, endNode).map(_.size) }
    .min - 1

  println(result) // 492



}
