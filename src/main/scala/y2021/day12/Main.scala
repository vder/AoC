package y2021.day12

import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration.*
import y2021.common.*
import cats.implicits.*
import fs2.Pipe
import fs2.Stream
import cats.data.State
import scala.annotation.tailrec
import fs2.Pull
import cats.effect.kernel.Sync
import y2021.day5.model.Point
import fs2.Chunk
import cats.Foldable.ops.toAllFoldableOps
import scala.util.Try
import scala.util.Success

object Main extends IOApp.Simple {

  type CaveMap = Map[String, Set[String]]

  val empty: CaveMap = Map()

  def part1VisitCondition(visited: List[String])(n: String) =
    n == n.toUpperCase || !visited.contains(n)

  def part2VisitCondition(visited: List[String])(n: String) =
    n == n.toUpperCase || !visited.contains(n) || {
      val smallvisited = visited.filter(n => n == n.toLowerCase)
      smallvisited.size == smallvisited.distinct.size
    }

  def cruise(
      m: CaveMap,
      c: String,
      f: List[String] => String => Boolean,
      visited: List[String],
      paths: List[List[String]]
  ): List[List[String]] =
    if (c == "end") (c :: visited) :: paths
    else {
      val nextDirections =
        m(c).filter(f(c::visited)).filterNot(_=="start")

      nextDirections.toList.flatMap(n => cruise(m, n, f, c :: visited, paths))
    }
  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day12.txt")
      .map { case s"$a-$b" => (a, b) }
      .fold(empty)((m, e) =>
        m + (e._1 -> (m.getOrElse(e._1, Set()) + e._2))
          + (e._2 -> (m.getOrElse(e._2, Set()) + e._1))
      )

  val part1 =
    lines.print
      .map(x => cruise(x, "start", part1VisitCondition, List(), List()))
      .map(_.size)
      .print
      .compile
      .drain

  val part2 =
    lines.print
      .map(x => cruise(x, "start", part2VisitCondition, List(), List()))
     // .map(_.size)
     //.print
      .map(_.size)
      .print
      .compile
      .drain
  val run = part2

}
