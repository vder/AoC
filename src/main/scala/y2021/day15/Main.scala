package y2021.day15

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

  @tailrec
  def calculateStep(
      m: Map[(Int, Int), Int],
      cost: Map[(Int, Int), Int],
      visited: Set[(Int, Int)],
      current: (Int, Int)
  ): Map[(Int, Int), Int] =
    if (visited == m.keySet) cost
    else {
      val neighbours = Set(
        (current._1 - 1, current._2),
        (current._1, current._2 + 1),
        (current._1 + 1, current._2),
        (current._1, current._2 - 1)
      )
      val newNeighbours = (neighbours intersect m.keySet) -- visited

      val newCosts = cost ++ newNeighbours
        .map(x => (x -> (cost(current) + m(x))))
        .toMap
        .filter((k, v) => v < cost.getOrElse(k, Int.MaxValue))
      val newCurrent = newCosts
        .filter((k, _) => k != current && !visited.contains(k))
        .toList
        .sortBy(_._2)
        .headOption
      newCurrent match {
        case Some(next) =>
          calculateStep(m, newCosts, visited + current, next._1)
        case None => newCosts
      }
    }

  //@tailrec
  def calculateStep2(
      m: Map[(Int, Int), Int],
      multi: Int,
      size: (Int, Int),
      cost: Map[(Int, Int), Int],
      visited: Set[(Int, Int)],
      current: (Int, Int)
  ): Map[(Int, Int), Int] = {
    def costCalc(x: (Int, Int)) = {
      (x._1 / size._1 + x._2 / size._2 + m(
        (x._1 % size._1, x._2 % size._2)
      ) - 1) % 9 + 1
    }
    def isInside(x: (Int, Int)) =
      x._1 >= 0 && x._2 >= 0 && x._1 < multi * size._1 && x._2 < multi * size._2

    if (visited == m.keySet) cost
    else {
      val neighbours = Set(
        (current._1 - 1, current._2),
        (current._1, current._2 + 1),
        (current._1 + 1, current._2),
        (current._1, current._2 - 1)
      )
      val newNeighbours = neighbours.filter(isInside) -- visited

      val newCosts = cost ++ newNeighbours
        .map(x => (x -> (cost(current) + costCalc(x))))
        .toMap
        .filter((k, v) => v < cost.getOrElse(k, Int.MaxValue))
      val newCurrent = newCosts
        .filter((k, _) => k != current && !visited.contains(k))
        .toList
        .sortBy(_._2)
        .headOption
      newCurrent match {
        case Some(next) =>
          calculateStep2(m, multi, size, newCosts, visited + current, next._1)
        case None => newCosts
      }

    }
  }

  val toMap: Vector[Vector[Int]] => Map[(Int, Int), Int] =
    v =>
      v.zipWithIndex.flatMap { case (v, i) =>
        v.zipWithIndex.map { case (v, j) => ((i, j) -> v) }
      }.toMap

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day15.txt")
      .map(_.toVector.map(_.asDigit))
      .fold(Vector[Vector[Int]]())((r, l) => r :+ l)
      .map(toMap)


      //7981 s solution
  val part1 =
    lines
      .map(x => (x, x.keySet.toList.sortBy { case (x, y) => -x - y }.head))
      .print
      .map { case (map, size) =>
        calculateStep2(
          map,
          5,
          (size._1 + 1, size._2 + 1),
          Map((0, 0) -> 0),
          Set(),
          (0, 0)
        )
      }
      .print
      .map(x => {
        val end = x.keySet.toList.sortBy { case (x, y) => -x - y }.head
        s"$end ${x(end)}"
      })
      .print
      .compile
      .drain

  val part2 =
    lines.print.compile.drain

  val run = part1

}
