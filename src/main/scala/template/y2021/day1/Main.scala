package y2021.day1

import cats.effect.{IO, IOApp}
import scala.concurrent.duration._
import template.y2021.common.FileHandler
import cats.implicits._
import fs2.Pipe

object Main extends IOApp.Simple {

  val toIntLines: Pipe[IO, String, Int] =
    _.evalMapFilter(x => IO { x.toIntOption })
  val diffPipe: Pipe[IO, Int, Int] = _.sliding(2)
    .map(x => x(1) - x(0))

  val lines = FileHandler[IO]
    .readFile("src/main/resources/day1_input.txt")
    .through(toIntLines)

  val run =
    (
      lines
        .through(diffPipe)
        .filter(_ > 0)
        .compile
        .count,
      lines
        .sliding(3)
        .map(_.toList.sum)
        .through(diffPipe)
        .filter(_ > 0)
        .compile
        .count
    ).tupled
      .flatMap(x => IO { println(x) })

}
