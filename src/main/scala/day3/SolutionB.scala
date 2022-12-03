package day3

import java.nio.file.{Files, Paths}

object SolutionB extends App {

  val input = Files.readString(Paths.get("src/main/resources/day3/input_a.txt"))

  val lines = input.split("\n")

  val result = lines
    .grouped(3)
    .map { _.map(_.toSet).reduce(_ intersect _).head }
    .map { overlap =>
      if (overlap.isUpper) overlap.toInt - 38
      else overlap.toInt - 96
    }
    .sum

  println(result) // 2631
}
