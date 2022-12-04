package day1

import java.nio.file.{Files, Paths}

object SolutionB extends App {

  val input = Files.readString(Paths.get("src/main/resources/day1/input_a.txt"))
  val groups = input.split("\n\n").map(_.split("\n").map(_.toInt))
  val calories = groups.map(_.sum).sorted(Ordering[Int].reverse).take(3).sum

  println(calories) // 207456
}
