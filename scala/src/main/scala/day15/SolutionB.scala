package day15

import util.collections.RichCollections._

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

object SolutionB extends App {

  val input = Files.readString(Paths.get("src/main/resources/day15/input_a.txt"))

//  val input =
//    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
//      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
//      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
//      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
//      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
//      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
//      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
//      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
//      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
//      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
//      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
//      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
//      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
//      |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  case class Coordinates(x: Int, y: Int) {
    def distanceTo(other: Coordinates): Int = math.abs(x - other.x) + math.abs(y - other.y)

    def frequency: Long = (x * 4000000L) + y
  }

  case class Range(from: Int, to: Int) extends Ordered[Range] {

    // println(Range(2,2).intersects(Range(-3,3)))
    def intersects(other: Range): Boolean =
      (other.from >= from && other.from <= to) || (other.to >= from && other.to <= to) ||
        (from >= other.from && from <= other.to) || (to >= other.from && to <= other.to)

    def touches(other: Range): Boolean =
      from - 1 == other.to || to + 1 == other.from

    def union(other: Range): Range = Range(math.min(from, other.from), math.max(to, other.to))

    override def compare(that: Range): Int = {
      val result = from.compare(that.from)
      if (result == 0) to.compare(that.to)
      else result
    }
  }

  val InputPattern = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

  val sensorsWithNearestBeacons = input
    .split('\n')
    .toList
    .map {
      case InputPattern(sensorX, sensorY, beaconX, beaconY) =>
        Coordinates(sensorX.toInt, sensorY.toInt) -> Coordinates(beaconX.toInt, beaconY.toInt)
    }

  def mergeRanges(ranges: List[Range]): List[Range] = {
    def loop(range: Range, remaining: List[Range]): List[Range] = remaining match {
      case Nil => List(range)
      case head :: tail if head.touches(range) || head.intersects(range) => loop(range.union(head), tail)
      case head :: tail => range :: loop(head, tail)
    }

    ranges.sorted match {
      case Nil => Nil
      case head :: tail => loop(head, tail)
    }
  }

  val rangesPerRow: Map[Int, List[Range]] = sensorsWithNearestBeacons
    .flatMap { case (sensor, beacon) =>
      val distance = sensor.distanceTo(beacon)
      for {
        yOff <- -distance to distance
      } yield {
        val xOff = distance - math.abs(yOff)
        val range = Range(sensor.x - xOff, sensor.x + xOff)

        sensor.y + yOff -> range
      }
    }
    .aggregatedByKey[List[Range]]

  rangesPerRow
    .map { case (y, ranges) => y -> mergeRanges(ranges) }
    .collect { case (y, List(a, _)) => Coordinates(a.to + 1, y) }
    .filter {  case Coordinates(x, y) => x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000 }
    .map(_.frequency)
    .foreach(println) // 12413999391794

}
