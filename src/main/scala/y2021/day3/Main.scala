package y2021.day3

import cats.effect.{IO, IOApp}
import scala.concurrent.duration.*
import y2021.common.FileHandler
import cats.implicits.*
import fs2.Pipe
import fs2.Stream
import cats.data.State
import scala.annotation.tailrec

object Main extends IOApp.Simple {

  val init = (0, Vector())

  val toCommands: Pipe[IO, String, Vector[Int]] =
    _.map(_.toVector.map(_.asDigit))

  val lines: Stream[IO, Vector[Int]] = FileHandler[IO]
    .readFile("src/main/resources/day3 copy.txt")
    .through(toCommands)

  def part1Controler(
      v: Vector[Int]
  ): State[(Int, Vector[Int]), Vector[Int]] = State { s =>
    {
      val newState =
        if (s._1 == 0)
          (1, v)
        else
          (s._1 + 1, (s._2.zip(v)).map { case (x, y) => x + y })
      (newState, newState._2)
    }
  }

  def popular[S](
      controler: Vector[Int] => State[(Int, Vector[Int]), Vector[Int]],
      initial: S,
      binaries: Stream[IO, Vector[Int]],
      most: Boolean
  ): Stream[IO, Vector[Int]] =
    binaries
      .map(controler)
      .reduce((c1, c2) => c1.flatMap(_ => c2))
      .evalMap(x => IO.eval(x.run(init)))
      .map { case ((cnt, _), v) => v.map(x => if (2 * x >= cnt) 1 else 0) }
      .map(v => v.map(x => if (most) x else 1 - x))

  def filter(
      l: Stream[IO, Vector[Int]],
      f: Vector[Int],
      i: Int,
      isMost: Boolean
  ): Stream[IO, Vector[Int]] = {
    val filtered = l.filter(_(i) == f(i))

    for {
      cnt <- Stream
        .eval(filtered.compile.count)
      most <- popular(part1Controler, init, filtered, isMost)
      result <-
        if (cnt == 1) filtered
        else filter(filtered, most, i + 1, isMost)
    } yield result
  }

  val part1 = popular(part1Controler, init, lines, true)
    .map(x => (x, x.invert))
    .map { case (g, e) =>
      (g.toInt, e.toInt, g.toInt * e.toInt)
    }
    .foreach(x => IO { println(x) })
    .compile
    .drain

  val part2 = {
    val filtered = for {
      f <- popular(part1Controler, init, lines, true)
      result1 <- filter(lines, f, 0, true)
      result2 <- filter(lines, f.invert, 0, false)
    } yield (result1, result2)

    filtered
      .evalTap(x =>
        IO {
          println(
            s"${x._1.toInt} -- ${x._2.toInt} = ${x._1.toInt * x._2.toInt}"
          )
        }
      )
      .compile
      .drain
  }

  val run = part1

  extension (v: Vector[Int]) {
    def invert = v.map(x => if (x == 1) 0 else 1)
    def toInt = v.toList.reverse.zipWithIndex.foldLeft(0.0) {
      case (res, (x, i)) =>
        res + Math.pow(2, i) * x
    }
  }
}
