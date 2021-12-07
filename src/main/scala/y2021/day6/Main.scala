package y2021.day6

import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration.*
import y2021.common.FileHandler
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

  val initial: Map[Int, Int] = Map(1 -> 1, 2 -> 1, 3 -> 2, 4 -> 1)

  val zero = BigInt(0)

  def simulate(s: Map[Int, BigInt], d: Int): Map[Int, BigInt] =
    if (d == 0) s
    else {
      val t = (0 to 7).toList.map(i => (i, s.getOrElse(i + 1, zero))).toMap
      val newState =
        t + (6 -> (t.getOrElse(6, zero) + s.getOrElse(0, zero)))
          + (8 -> s.getOrElse(0, zero))
      simulate(newState, d - 1)
    }

  def simulateStream(s: Map[Int, BigInt]): Map[Int, BigInt] = {
    val t = (0 to 7).toList.map(i => (i, s.getOrElse(i + 1, zero))).toMap
    val newState =
      t + (6 -> (t.getOrElse(6, zero) + s.getOrElse(0, zero)))
        + (8 -> s.getOrElse(0, zero))
    newState
  }

  val lines: Stream[IO, String] =
    FileHandler[IO]
      .readFile("src/main/resources/day6.txt")

  val part1 =
    lines
      .take(1)
      .flatMap(s => Stream.emits(s.split(",").toList))
      .map(_.toInt)
      .fold(Map[Int, BigInt]())((m, age) =>
        m + (age -> (m.getOrElse(age, zero) + 1))
      )
      .flatMap(s =>
        Stream.unfold(s)(st => {
          val state = simulateStream(st)
          Some((state, state))
        })
      )
      .evalTap(x => IO { println(s"${x.values.sum} =   $x ") })
      .take(256)
      .compile
      .drain

  val run = part1
}
