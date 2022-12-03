package day3

import java.nio.file.{Files, Paths}

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day3/input_a.txt"))

  val lines = input.split("\n")

  val result = lines
    .map { line =>
      line.splitAt(line.length / 2)
    }
    .map { case (a, b) =>
      a.toSet.intersect(b.toSet).head
    }
    .map { overlap =>
      if (overlap.isUpper) overlap.toInt - 38
      else overlap.toInt - 96
    }
    .sum

  println(result) // 8243
}
