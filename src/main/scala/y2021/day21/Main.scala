package y2021.day21

import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration.*
import y2021.common.*
import cats.implicits.*
import cats.implicits.given
import fs2.Pipe
import fs2.Stream
import cats.data.State
import scala.annotation.tailrec
import fs2.Pull
import cats.effect.kernel.Sync
import scala.language.postfixOps
import fs2.Chunk
import cats.effect.std.Queue
import cats.instances.all.given
object Main extends IOApp.Simple {

  case class Player(id: Int, score: Int, pos: Int)
  case class Roll(id: Long, value: Int)

  def dice: Stream[IO, Int] = Stream.emits(1 to 100) ++ dice

  def rolls(n: Int): Pipe[IO, (Int, Long), Roll] = {

    val groups = 10
    val arr = Array.fill[Int](groups)(0)

    def go(
        s: Stream[IO, (Int, Long)]
    ): Pull[IO, Roll, Unit] = {

      s.pull.unconsN(n * groups).flatMap {
        case Some(rolls, rest) =>
          Pull.output(
            Chunk.iterable[Roll](
              rolls.toList
                .sliding(n, n)
                .map(x => Roll(x.map(_._2).max + 1, x.map(_._1).sum))
                .to(Iterable)
            )
          ) >> go(rest)
        case None =>
          Pull.done
      }
    }
    s => go(s).stream
  }

  val board = Array(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  val part1 =
    dice.zipWithIndex
      .through(rolls(3))
      .scan((Player(1, 0, 3), Player(2, 0, 7), Roll(0, 0))) {
        case ((p1, p2, ro), r @ Roll(id, value)) => {
          val newPos = (p1.pos + value) % 10
          (p2, p1.copy(score = p1.score + board(newPos), pos = newPos), r)
        }
      }
      .print
      .takeWhile((p1, p2, ro) => p2.score < 1000)
      .compile
      .drain

  val rf = List((3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1))

  def wins(p1: Int, t1: Int, p2: Int, t2: Int): (Long, Long) =
    if (t2 <= 0) (0, 1)
    else {
      var (w1, w2) = (0L, 0L)
      rf.foreach { case (r, f) =>
        val (c2, c1) = wins(p2, t2, (p1 + r) % 10, t1 - 1 - (p1 + r) % 10)
        w1 = w1 + f * c1
        w2 = w2 + f * c2
      }
      (w1, w2)
    }

  val run = Stream.emit(wins(2, 21, 6, 21)).covary[IO].print.compile.drain

}
