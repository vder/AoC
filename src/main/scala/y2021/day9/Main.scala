package y2021.day9

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

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day9.txt")
      .map(_.toVector.map(_.asDigit))
      .fold(Vector[Vector[Int]]())((r, l) => r :+ l)

  val part1 = lines
    .map(v => {
      val locals = for {
        i <- 0 until v.size
        j <- 0 until v(i).size
        if v(i)(j) < v.neighbourValues(i, j).min
      } yield (i, j)

      locals.map(x => (x, v(x._1)(x._2))).toMap
    })
    .map(_.values.map(_ + 1).sum)
    .print
    .compile
    .drain

  val part2 =
    lines
      .map(v => {
        val locals = for {
          i <- 0 until v.size
          j <- 0 until v(i).size
          if v(i)(j) < v.neighbourValues(i, j).min
        } yield (i, j)

        locals.map(x => (x, v(x._1)(x._2))).toMap
      })
      .flatMap(x => (Stream.emits(x.keys.toList)))
      .map(x =>
        (
          x,
          lines.flatMap(basinMap =>
            Stream.unfold((List(x), List[(Int, Int)]())) {
              case (toVisit, visited) =>
                for {
                  current <- toVisit.headOption
                  newNeigborous = basinMap
                    .neighbours(current._1, current._2)
                    .filterNot(_._2 == 9)
                    .map(_._1)
                    .filterNot(visited.contains(_))
                    .filterNot(toVisit.tail.contains(_))
                    .filterNot(_ == current)
                } yield (
                  current,
                  (newNeigborous ::: toVisit.tail, current :: visited)
                )
            } 
          )
        )
      )
      .parEvalMap(8) { case (start, stream) =>
        (IO.pure(start), stream.compile.count).tupled
      }
      .print
      .compile
      .toList
      .map(_.map(_._2).sortBy(-_).take(3).product)
      .flatMap(x => IO { println(x) })
      .void

  val run = part2

  extension (v: Vector[Vector[Int]]) {

    private def getValue(x: Int, y: Int): List[Int] = Try(v(x)(y)) match {
      case Success(x) => List(x)
      case _          => Nil
    }
    def neighbours(x: Int, y: Int): List[((Int, Int), Int)] = {
      val points = List((x - 1, y),(x + 1, y),(x, y - 1),(x, y + 1))

      points.collect {
        case p @ (x, y) if !getValue(x, y).isEmpty => (p, getValue(x, y).head)
      }
    }

    def neighbourValues(x: Int, y: Int): List[Int] =
       List((x - 1, y),(x + 1, y),(x, y - 1),(x, y + 1)).flatMap(getValue(_,_))
  }
}
