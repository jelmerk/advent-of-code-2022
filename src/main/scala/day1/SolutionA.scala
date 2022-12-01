package day1

import java.nio.file.{Files, Paths}

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day1/input_a.txt"))
  val groups = input.split("\n\n").map(_.split("\n").map(_.toInt))
  val mostCalories = groups.map(_.sum).max

  println(mostCalories) // 69177

}
