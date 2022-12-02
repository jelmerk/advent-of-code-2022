package day2

import java.nio.file.{Files, Paths}

object SolutionB extends App {

  sealed trait Choice

  object Choice {
    case object Rock extends Choice

    case object Paper extends Choice

    case object Scissors extends Choice
  }

  sealed trait Outcome

  object Outcome {
    case object Lose extends Outcome

    case object Draw extends Outcome

    case object Win extends Outcome
  }

  val columnAMapping: Map[String, Choice] = Map(
    "A" -> Choice.Rock,
    "B" -> Choice.Paper,
    "C" -> Choice.Scissors
  )

  val columnBMapping: Map[String, Outcome] = Map(
    "X" -> Outcome.Lose,
    "Y" -> Outcome.Draw,
    "Z" -> Outcome.Win
  )

  val shapeScores: Map[Choice, Int] = Map(
    Choice.Rock     -> 1,
    Choice.Paper    -> 2,
    Choice.Scissors -> 3
  )

  def roundScore(me: Choice, them: Choice) = (me, them) match {
    case (Choice.Rock, Choice.Scissors)     => 6
    case (Choice.Rock, Choice.Rock)         => 3
    case (Choice.Rock, Choice.Paper)        => 0
    case (Choice.Paper, Choice.Rock)        => 6
    case (Choice.Paper, Choice.Paper)       => 3
    case (Choice.Paper, Choice.Scissors)    => 0
    case (Choice.Scissors, Choice.Paper)    => 6
    case (Choice.Scissors, Choice.Scissors) => 3
    case (Choice.Scissors, Choice.Rock)     => 0
  }

  def findPlay(me: Choice, outcome: Outcome) = (me, outcome) match {
    case (Choice.Rock, Outcome.Lose)     => Choice.Scissors
    case (Choice.Rock, Outcome.Win)      => Choice.Paper
    case (Choice.Rock, Outcome.Draw)     => Choice.Rock
    case (Choice.Paper, Outcome.Lose)    => Choice.Rock
    case (Choice.Paper, Outcome.Win)     => Choice.Scissors
    case (Choice.Paper, Outcome.Draw)    => Choice.Paper
    case (Choice.Scissors, Outcome.Lose) => Choice.Paper
    case (Choice.Scissors, Outcome.Win)  => Choice.Rock
    case (Choice.Scissors, Outcome.Draw) => Choice.Scissors

  }

  val input = Files.readString(Paths.get("src/main/resources/day2/input_a.txt"))

  val rounds = input
    .split("\n")
    .map(_.split(' '))
    .map { case Array(a, b) => columnAMapping(a) -> columnBMapping(b) }

  val result = rounds
    .map { case (them, outcome) =>
      val me = findPlay(them, outcome)
      val shapeScore = shapeScores(me)
      val score = roundScore(me, them)
      shapeScore + score
    }
    .sum

  println(result) // 16862


}
