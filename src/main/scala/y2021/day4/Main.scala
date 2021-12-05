package y2021.day4

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

object Main extends IOApp.Simple {

  def toBoards: Pipe[IO, String, Board] = {
    def go(
        s: Stream[IO, String],
        v: Vector[Vector[Int]]
    ): Pull[IO, Board, Unit] = {
      s.pull.uncons1.flatMap {
        case Some(line, rest) =>
          if (line.isEmpty)
            if (v.isEmpty) go(rest, v)
            else Pull.output1(Board(v)) >> go(rest, Vector())
          else
            go(
              rest,
              v :+ line.split(" ").filterNot(_.isEmpty).map(_.toInt).toVector
            )
        case None =>
          if (v.isEmpty) Pull.done
          else Pull.output1(Board(v)) >> Pull.done
      }
    }
    in => go(in, Vector()).stream
  }

  val lines: (Stream[IO, Int], Stream[IO, Board]) = {
    val l = FileHandler[IO]
      .readFile("src/main/resources/day4.txt")
    (
      l.take(1).flatMap(x => Stream.emits(x.split(",").toList.map(_.toInt))),
      l.drop(1).through(toBoards)
    )
  }

  val part1 = IO { println("sdfsd") }
  val run = {
    val (shotsStream, boardStream) = lines
    val boardsIO = boardStream.compile.toList

    for {
      previous <- Ref[IO].of(List[Int]())
      boards <- boardsIO
      _ <- shotsStream.zipWithNext
        .evalMap {
          case (i, Some(next)) =>
            for {
              taken <- previous.updateAndGet(prev => i :: prev)
              lost = boards.filterNot(_.isWin(taken.toSet))
            } yield (next :: taken, lost)
          case _ => IO{(Nil, Nil)}
        }
        .filterNot(_._2.isEmpty)
        .last
        .evalMap {
          case Some(x) =>
            IO {
              println(s"${x._2.head.unmarked(x._1.toSet).sum * x._1.head}")
            }
          case _ => IO { () }
        }
        .compile
        .drain
    } yield ()

  }
}
