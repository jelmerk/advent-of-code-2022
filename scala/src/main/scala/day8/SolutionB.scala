package day8

import java.nio.file.{Files, Paths}
object SolutionB extends App {

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


    def scenicScore(x: Int, y: Int): Long = {
      val treeValue = values(y)(x)

      val leftScore = {
        val possible = Range.inclusive(x - 1, 0, -1)
        val lower = possible.takeWhile(xPos => values(y)(xPos) < treeValue)
        lower.size + (if (lower.size < possible.size) 1 else 0)
      }

      val rightScore = {
        val possible = Range.inclusive(x + 1, width - 1)
        val lower = possible.takeWhile(xPos => values(y)(xPos) < treeValue)
        lower.size + (if (lower.size < possible.size) 1 else 0)
      }

      val topScore = {
        val possible = Range.inclusive(y - 1, 0, -1)
        val lower = possible.takeWhile(yPos => values(yPos)(x) < treeValue)
        lower.size + (if (lower.size < possible.size) 1 else 0)
      }

      val bottomScore = {
        val possible = Range.inclusive(y + 1, height - 1)
        val lower = possible.takeWhile(yPos => values(yPos)(x) < treeValue)
        lower.size + (if (lower.size < possible.size) 1 else 0)
      }

      List(leftScore, rightScore, topScore, bottomScore).product
    }

    override def toString: String = values.map(_.mkString("")).mkString("\n")
  }

  val grid = Grid.create(input)

  val result = (for {
    y <- 0 until grid.height
    x <- 0 until grid.width
  } yield grid.scenicScore(x, y)).max

  println(result) // 211680




}
