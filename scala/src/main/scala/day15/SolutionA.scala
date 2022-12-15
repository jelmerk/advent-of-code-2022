package day15

import java.nio.file.{Files, Paths}

object SolutionA extends App {

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
  }

  val InputPattern = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r

  val sensorsWithNearestBeacons = input
    .split('\n')
    .map {
      case InputPattern(sensorX, sensorY, beaconX, beaconY) =>
        Coordinates(sensorX.toInt, sensorY.toInt) -> Coordinates(beaconX.toInt, beaconY.toInt)
    }


  val noBeaconCoordinates = sensorsWithNearestBeacons
    .flatMap { case (sensor, beacon) =>
      val distance = sensor.distanceTo(beacon)

      for {
        y <- (sensor.y - distance to sensor.y + distance).filter(_ == 2000000)
        x <- sensor.x - distance to sensor.x + distance

        coordinates = Coordinates(x, y)
        if sensor.distanceTo(coordinates) <= distance
        if coordinates != beacon
      } yield coordinates
    }
    .toSet

  println(noBeaconCoordinates.size) // 5838453

}
