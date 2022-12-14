package day13

import java.nio.file.{Files, Paths}

object SolutionA extends App {

  val input = Files.readString(Paths.get("src/main/resources/day13/input_a.txt"))

//  val input =
//    """[1,1,3,1,1]
//      |[1,1,5,1,1]
//      |
//      |[[1],[2,3,4]]
//      |[[1],4]
//      |
//      |[9]
//      |[[8,7,6]]
//      |
//      |[[4,4],4,4]
//      |[[4,4],4,4,4]
//      |
//      |[7,7,7,7]
//      |[7,7,7]
//      |
//      |[]
//      |[3]
//      |
//      |[[[]]]
//      |[[]]
//      |
//      |[1,[2,[3,[4,[5,6,7]]]],8,9]
//      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

  sealed trait Element
  case class Literal(value: Int) extends Element {
    override def toString: String = value.toString
  }
  case class ListOfValues(values: List[Element]) extends Element {
    override def toString: String = s"[${values.mkString(",")}]"
  }

  def parse(input: String): ListOfValues = {

    def parseInput(chars: List[Char], stack: List[ListOfValues]): ListOfValues = chars match {
      case '[' :: tail => parseInput(tail, ListOfValues(List.empty) :: stack)
      case ']' :: Nil  => stack.head
      case ']' :: tail =>
        val List(collected, ListOfValues(elements), rest @ _*) = stack
        parseInput(tail,  ListOfValues(elements :+ collected) :: rest.toList )
      case ',' :: tail => parseInput(tail, stack)
      case ' ' :: tail => parseInput(tail, stack)
      case list =>
        val (taken, remaining) = list.span(_.isDigit)
        val value = taken.mkString.toInt
        parseInput(remaining, ListOfValues(stack.head.values :+ Literal(value)) :: stack.tail)
    }

    parseInput(input.toList, List.empty[ListOfValues])
  }

  sealed trait TriStateBoolean

  object TriStateBoolean {
    case object True extends TriStateBoolean
    case object False extends TriStateBoolean
    case object Undefined extends TriStateBoolean
  }

  def isInRightOrder(left: Element, right: Element): TriStateBoolean = (left, right) match  {
    case (l: Literal,r: ListOfValues) => isInRightOrder(ListOfValues(List(Literal(l.value))), r)
    case (l: ListOfValues , r: Literal) => isInRightOrder(l, ListOfValues(List(Literal(r.value))))
    case (l: ListOfValues, r: ListOfValues) =>
      l.values.zip(r.values)
        .map { case (ll, rr) => isInRightOrder(ll, rr) }
        .filterNot { _ == TriStateBoolean.Undefined }
        .headOption
        .getOrElse {
          if (l.values.size == r.values.size) TriStateBoolean.Undefined
          else if (l.values.size < r.values.size) TriStateBoolean.True
          else TriStateBoolean.False
        }

      // If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
    case (l: Literal , r: Literal) =>
      if (l.value < r.value) TriStateBoolean.True
      else if (l.value > r.value) TriStateBoolean.False
      else TriStateBoolean.Undefined
  }

  val pairs = input
    .split("\n\n")
    .map(_.split("\n"))
    .map { case Array(line1, line2) => parse(line1) -> parse(line2) }

  val result = pairs
    .map { case (left, right) => isInRightOrder(left, right) }
    .zip(Stream.from(1))
    .collect {
      case (TriStateBoolean.True, index) => index
    }
    .sum

  println(result) // 5350

}
