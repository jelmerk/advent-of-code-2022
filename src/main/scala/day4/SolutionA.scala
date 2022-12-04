package day4

import java.nio.file.{Files, Paths}

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day4/input_a.txt"))

  case class Range(lower: Int, upper: Int) {
    def fullyOverlapsWith(other: Range): Boolean = other.lower >= lower && other.upper <= upper
  }

  val ranges = input
    .split('\n')
    .map(_.split(','))
    .map { case Array(a, b) =>
      val Array(lowerA, upperA) = a.split('-')
      val Array(lowerB, upperB) = b.split('-')

      Range(lowerA.toInt, upperA.toInt) -> Range(lowerB.toInt, upperB.toInt)
    }

  val result = ranges
    .map {
      case (range1, range2) =>
        if (range1.fullyOverlapsWith(range2) || range2.fullyOverlapsWith(range1)) 1
        else 0
    }
    .sum

  println(result)

}
