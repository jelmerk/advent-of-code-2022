package day13

import java.nio.file.{Files, Paths}

object SolutionB extends App {

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
  case class ListOfValues(values: List[Element]) extends Element  with Comparable[ListOfValues] {
    override def toString: String = s"[${values.mkString(",")}]"

    override def compareTo(other: ListOfValues): Int = {
      def compare(left: Element, right: Element): Int = (left, right) match {
        case (l: Literal, r: ListOfValues) => compare(ListOfValues(List(Literal(l.value))), r)
        case (l: ListOfValues, r: Literal) => compare(l, ListOfValues(List(Literal(r.value))))
        case (l: ListOfValues, r: ListOfValues) =>
          l.values.zip(r.values)
            .map { case (ll, rr) => compare(ll, rr) }
            .filterNot(_ == 0)
            .headOption
            .getOrElse {
              if (l.values.size == r.values.size) 0
              else if (l.values.size < r.values.size) 1
              else -1
            }
        case (l: Literal, r: Literal) =>
          if (l.value < r.value) 1
          else if (l.value > r.value) -1
          else 0
      }

      compare(other, this)
    }
  }

  def parse(input: String): ListOfValues = {

    def parseInput(chars: List[Char], stack: List[ListOfValues]): ListOfValues = chars match {
      case '[' :: tail => parseInput(tail, ListOfValues(List.empty) :: stack)
      case ']' :: Nil  => stack.head
      case ']' :: tail =>
        val List(collected, ListOfValues(elements), rest @ _*) = stack
        parseInput(tail,  ListOfValues(elements :+ collected) :: rest.toList )
      case (',' | ' ') :: tail => parseInput(tail, stack)
      case list =>
        val (taken, remaining) = list.span(_.isDigit)
        val value = taken.mkString.toInt
        parseInput(remaining, ListOfValues(stack.head.values :+ Literal(value)) :: stack.tail)
    }

    parseInput(input.toList, List.empty[ListOfValues])
  }

  val packets = input
    .split("\n\n")
    .flatMap(_.split("\n"))
    .map(parse)
    .toList

  val insert1 = parse("[[2]]")
  val insert2 = parse("[[6]]")

  val inserts = List(insert1, insert2)

  val result = (inserts ::: packets)
    .sorted
    .zip(Stream.from(1))
    .collect {
      case (packet, index) if inserts.contains(packet) => index
    }
    .product

  println(result) // 19570

}
