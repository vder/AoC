package y2021.day5

import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration.*
import y2021.common.FileHandler
import cats.implicits.*
import fs2.Pipe
import fs2.Stream
import cats.data.State
import scala.annotation.tailrec
import y2021.day4.model.Board
import fs2.Pull
import y2021.day4.model.Board
import cats.effect.kernel.Sync
import y2021.day5.model.Point
import fs2.Chunk

object Main extends IOApp.Simple {

  val toLines: Pipe[IO, String, Option[(Point, Point)]] =
    _.map(_ match {
      case s"$x1s,$y1s -> $x2s,$y2s" =>
        for {
          x1 <- x1s.toIntOption
          x2 <- x2s.toIntOption
          y1 <- y1s.toIntOption
          y2 <- y2s.toIntOption
        } yield (Point(x1, y1), Point(x2, y2))
      case _ => None
    })

  def toPoints: Pipe[IO, (Point, Point), Point] = {

    def go(
        stream: Stream[IO, (Point, Point)]
    ): Pull[IO, Point, Unit] = {
      stream.pull.uncons.flatMap {
        case Some(line, rest) =>
          Pull.output(
            line.flatMap(l => Chunk(Point.makeLine(l._1, l._2)*))
          ) >> go(rest)
        case None => Pull.done
      }
    }
    in => go(in).stream
  }

  val lines: Stream[IO, (Point, Point)] = {
    FileHandler[IO]
      .readFile("src/main/resources/day5.txt")
      .through(toLines)
      .collect { case Some(x) => x }

  }

  val part1 =
    lines
      .through(toPoints)
      .fold(Map[Point, Int]())((m, p) => m + (p -> (m.getOrElse(p, 0) + 1)))
      .evalTap(x => IO { println(x.filter(_._2 > 1).size) })
      .compile
      .drain
  val run = part1
}
