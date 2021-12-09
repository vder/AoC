package y2021.day7

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

object Main extends IOApp.Simple {

  val lines: Stream[IO, List[Int]] =
    FileHandler[IO]
      .readFile("src/main/resources/day7.txt")
      .map(_.split(',').toList.map(_.toInt))

  val part1 =
    (
      lines,
      lines
        .map(x => (x.min, x.max))
        .flatMap(x => Stream.emits(x._1 to x._2))
    ).tupled
      .map { case (list, dest) =>
        (dest, list.map(x => Math.abs(x - dest)).sum)
      }
      .compile
      .toList
      .flatMap(x => IO { println(x.filter(_._2 == x.map(_._2).min)) })
      .void

  val unFoldStream = Stream.unfold((0, 0)) { case (i, s) =>
    Some((i + 1, s + i + 1), (i + 1, s + i + 1))
  }

  val costMapStream = unFoldStream
    .covary[IO]

    

  val part2 =
    (
      lines,
      lines
        .map(x => (x.min, x.max))
        .flatMap(x => Stream.emits(x._1 to x._2))
    ).tupled
      .evalMap { case (list, dest) =>{
      val maxDistance = list.map(x => Math.abs(x - dest)).max
      val costsMapIO = costMapStream.take(maxDistance).compile.toList.map(_.toMap).map(_+ (0->0))
      costsMapIO.map(m =>  (dest, list.map(x => m(Math.abs(x - dest))).sum))}
      }
      .compile
      .toList
      .flatMap(x => IO { println(x.filter(_._2 == x.map(_._2).min)) })
      .void
  val run = part2
}
