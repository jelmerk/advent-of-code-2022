package day6

import java.nio.file.{Files, Paths}

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day6/input_a.txt"))

  val windowSize = 4

  input
    .sliding(windowSize)
    .zipWithIndex
    .collectFirst { case (letters, index) if letters.distinct.length == windowSize => index + windowSize }
    .foreach(println) // 1582
}
