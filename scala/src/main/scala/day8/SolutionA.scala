package day8

import java.nio.file.{Files, Paths}
object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day8/input_a.txt"))

//  val input =
//    """30373
//      |25512
//      |65332
//      |33549
//      |35390""".stripMargin

  object Grid {
    def create(input: String): Grid = {
      val values = input.split('\n').map(_.toArray.map(_.toString.toInt))
      new Grid(values)
    }
  }
  class Grid private (values: Array[Array[Int]]) {

    val height: Int = values.size

    val width: Int = values.headOption.map(_.length).getOrElse(0)

    def isVisible(x: Int, y: Int): Boolean = {
      val atleftEdge = x == 0
      val atTopEdge = y == 0
      val atRightEdge = x == width - 1
      val atBottomEdge = y == height - 1

//      if (atleftEdge || atTopEdge || atRightEdge || atBottomEdge) false
      if (atleftEdge || atTopEdge || atRightEdge || atBottomEdge) true
      else {
        val treeValue = values(y)(x)
        val visibleFromLeft = Range.inclusive(x - 1, 0, -1).forall(xPos => values(y)(xPos) < treeValue)
        val visibleFromRight = Range.inclusive(x + 1, width - 1).forall(xPos => values(y)(xPos) < treeValue)
        val visibleFromTop = Range.inclusive(y - 1, 0, -1).forall(yPos => values(yPos)(x) < treeValue)
        val visibleFromBottom = Range.inclusive(y + 1, height - 1).forall(yPos => values(yPos)(x) < treeValue)

        visibleFromLeft || visibleFromRight || visibleFromTop || visibleFromBottom
      }
    }

    override def toString: String = values.map(_.mkString("")).mkString("\n")
  }

  val grid = Grid.create(input)

  val result = (for {
    y <- 0 until grid.height
    x <- 0 until grid.width
    if grid.isVisible(x, y)
  } yield 1).sum

  println(result) // 1823



}
