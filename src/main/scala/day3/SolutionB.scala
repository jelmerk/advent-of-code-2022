package day3

import java.nio.file.{Files, Paths}

object SolutionB extends App {

  val input = Files.readString(Paths.get("src/main/resources/day3/input_a.txt"))

  val lines = input.split('\n')

  def findIntersection(grouped: Array[String]): Option[Char] = grouped
    .map(_.toSet)
    .reduceOption(_ intersect _)
    .flatMap(_.headOption)

  def toValue(character: Char): Int =
    if (character.isUpper) character.toInt - 38
    else character.toInt - 96

  val result = lines
    .grouped(3)
    .flatMap(findIntersection)
    .map(toValue)
    .sum

  println(result) // 2631
}
