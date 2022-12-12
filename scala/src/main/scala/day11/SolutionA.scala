package day11


import java.nio.file.{Files, Paths}


object SolutionA extends App {


  val input = Files.readString(Paths.get("src/main/resources/day11/input_a.txt"))

//  val input =
//    """Monkey 0:
//      |  Starting items: 79, 98
//      |  Operation: new = old * 19
//      |  Test: divisible by 23
//      |    If true: throw to monkey 2
//      |    If false: throw to monkey 3
//      |
//      |Monkey 1:
//      |  Starting items: 54, 65, 75, 74
//      |  Operation: new = old + 6
//      |  Test: divisible by 19
//      |    If true: throw to monkey 2
//      |    If false: throw to monkey 0
//      |
//      |Monkey 2:
//      |  Starting items: 79, 60, 97
//      |  Operation: new = old * old
//      |  Test: divisible by 13
//      |    If true: throw to monkey 1
//      |    If false: throw to monkey 3
//      |
//      |Monkey 3:
//      |  Starting items: 74
//      |  Operation: new = old + 3
//      |  Test: divisible by 17
//      |    If true: throw to monkey 0
//      |    If false: throw to monkey 1""".stripMargin

  sealed trait Token

  sealed trait Operator extends Token

  case object Multiply extends Operator

  case object Plus extends Operator


  sealed trait Expression extends Token

  case class Literal(value: Int) extends Expression

  case class Variable(name: String) extends Expression

  case class Statement(left: Expression, operator: Operator, right: Expression) extends Expression

  case class Stage(id: Int, expression: Expression, divider: Int, pass: Int, fail: Int)

  def parseSimple(expression: String): Statement = {
    val Array(left, operation, right) = expression.split(' ')

    val leftExr =
      if (left.forall(_.isDigit)) Literal(left.toInt)
      else Variable(left)

    val rightExpr =
      if (right.forall(_.isDigit)) Literal(right.toInt)
      else Variable(right)

    val op = operation match {
      case "*" => Multiply
      case "+" => Plus
    }

    Statement(leftExr, op, rightExpr)
  }

  def evaluate(expression: Expression, variables: Map[String, Long]): Long = expression match {
    case Literal(value) => value
    case Variable(name) => variables(name)
    case Statement(left, Multiply, right) => evaluate(left, variables) * evaluate(right, variables)
    case Statement(left, Plus, right) => evaluate(left, variables) + evaluate(right, variables)
  }

  case class State(inventory: Map[Int, List[Long]], inspectionCounts: Map[Int, Long])

  val groups = input
    .split("\n\n")
    .map(_.split("\n"))
    .toList

  val stages: List[Stage] = groups
    .map { lines =>
      val id = lines(0).drop(7).dropRight(1).toInt
      val operationString = lines(2).drop(13).drop(6)
      val divider = lines(3).drop(21).toInt
      val passMonkey = lines(4).drop(29).toInt
      val failMonkey = lines(5).drop(30).toInt

      val expression = parseSimple(operationString)

      Stage(id, expression, divider, passMonkey, failMonkey)
    }

  val inventory: Map[Int, List[Long]] = groups
    .map { lines =>
      val id = lines(0).drop(7).dropRight(1).toInt
      val items = lines(1).drop(18).split(',').map(_.trim.toLong).toList

      id -> items
    }
    .toMap

  val initialState = State(inventory, Map.empty)
  val numRounds = 20

  val endState = (1 to numRounds).foldLeft(initialState) { case (roundState, _) =>
    stages.foldLeft(roundState) {
      case (State(inventory, inspectionCounts), Stage(id, expression, divider, pass, fail)) =>

        val items = inventory(id)

        val newInventory = items.foldLeft(inventory.updated(id, List.empty)) { case (acc, item) =>
          val result = evaluate(expression, variables = Map("old" -> item))

          val boredWorryLevel = result / 3

          val passTo =
            if (boredWorryLevel % divider == 0) pass
            else fail

          acc
            .updated(passTo,  acc(passTo) :+ boredWorryLevel)
        }

        val newInspectionCounts = inspectionCounts
          .updated(id, inspectionCounts.getOrElse(id, 0L) + items.size)

        State(newInventory, newInspectionCounts)
      }
  }

  val result = endState.inspectionCounts
    .values
    .toList
    .sorted(Ordering[Long].reverse)
    .take(2)
    .product

  println(result) // 117624


}
