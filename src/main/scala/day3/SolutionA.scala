package day3

import java.nio.file.{Files, Paths}

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day3/input_a.txt"))

  val lines = input.split('\n')

  def splitInHalf(line: String): (String, String) = line.splitAt(line.length / 2)

  def findIntersection(a: String, b: String): Option[Char] = a.toSet.intersect(b.toSet).headOption

  def toValue(character: Char): Int =
    if (character.isUpper) character.toInt - 38
    else character.toInt - 96

  val result = lines
    .map(splitInHalf)
    .flatMap { case (a, b) => findIntersection(a, b) }
    .map(toValue)
    .sum

  println(result) // 8243
}
