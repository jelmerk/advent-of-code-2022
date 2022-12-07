package day7

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec


object SolutionA extends App {

  val ChangeDirectoryCommandString = "\\$ cd (.*)".r
  val ListCommandString = "\\$ ls".r

  val FileString = "(\\d+) (.*)".r
  val DirectoryString = "dir (.*)".r

  val input = Files.readString(Paths.get("src/main/resources/day7/input_a.txt"))

//  val input = """$ cd /
//                |$ ls
//                |dir a
//                |14848514 b.txt
//                |8504156 c.dat
//                |dir d
//                |$ cd a
//                |$ ls
//                |dir e
//                |29116 f
//                |2557 g
//                |62596 h.lst
//                |$ cd e
//                |$ ls
//                |584 i
//                |$ cd ..
//                |$ cd ..
//                |$ cd d
//                |$ ls
//                |4060174 j
//                |8033020 d.log
//                |5626152 d.ext
//                |7214296 k""".stripMargin

  sealed trait Node
  case class Directory(path: List[String]) extends Node
  case class File(size: Long, name: String) extends Node

  val lines = input
    .split('\n')
    .toList

  @tailrec def process(inputs: List[String],
                       path: List[String],
                       directories: Map[List[String], List[Node]]): Map[List[String], List[Node]] = inputs match {
    case Nil => directories
    case input :: tail =>
      input match {
        case ChangeDirectoryCommandString("..") => process(tail, path.dropRight(1), directories)
        case ChangeDirectoryCommandString(name) => process(tail, path :+ name, directories)
        case ListCommandString() => process(tail, path, directories)
        case FileString(size, name) => process(tail, path, directories.updated(path, directories.getOrElse(path, List.empty) :+ File(size.toLong, name)))
        case DirectoryString(name) => process(tail, path, directories.updated(path, directories.getOrElse(path, List.empty) :+ Directory(path:+ name)))
      }
  }

  def resolveSize(directoryPath: List[String], directories: Map[List[String], List[Node]]): Long = directories(directoryPath)
    .map {
      case File(size, _) => size
      case Directory(path) => resolveSize(path, directories)
    }
    .sum

  val directories = process(lines, List.empty, Map.empty)

  val result = directories
    .keys
    .toList
    .map { path => resolveSize(path, directories) }
    .filter(_ <= 100000)
    .sum

  println(result) // 1501149



}
