package y2021.day13

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

  enum Axis {
    case X, Y
  }

  import Axis.*

  case class Fold(axis: Axis, value: Int)

  def show(s: Set[(Int, Int)]) = {
    val lines = for {
      y <- (0 to s.map(_._2).max).toList
      line = for {
        x <- (0 to s.map(_._1).max).toList
      } yield (if (s.contains((x, y))) '#' else '.')
    } yield line

    lines.map(_.mkString)
  }
  def foldHorizontal(page: Set[(Int, Int)], pos: Int): Set[(Int, Int)] =
    page.filter((x, _) => x < pos)
      ++ page.filter((x, _) => x > pos).map((x, y) => (2 * pos - x, y))

  def foldVertical(page: Set[(Int, Int)], pos: Int): Set[(Int, Int)] =
    page.filter((_, y) => y < pos)
      ++ page.filter((_, y) => y > pos).map((x, y) => (x, 2 * pos - y))

  def foldAlong(page: Set[(Int, Int)], f: Fold) =
    f match {
      case Fold(X, a) => foldHorizontal(page, a)
      case Fold(Y, a) => foldVertical(page, a)
    }

  val lines =
    (
      FileHandler[IO]
        .readFile("src/main/resources/day13.txt")
        .collect { case s"$a,$b" => (a.toInt, b.toInt) }
        .fold(Set[(Int, Int)]())((s, e) => s + e),
      FileHandler[IO]
        .readFile("src/main/resources/day13.txt")
        .collect {
          case s"fold along x=$x" => Fold(X, x.toInt)
          case s"fold along y=$x" => Fold(Y, x.toInt)
        }
        .fold(List[Fold]())((l, e) => e :: l)
        .map(_.reverse)
    ).tupled

  val part1 =
    lines.print
      .map((page, foldList) => foldAlong(page, foldList.head))
      .print
      .map(_.size)
      .print
      .compile
      .drain

  val part2 =
    lines.print
      .map((page, foldList) =>
        foldList.foldLeft(page)((s, f) => foldAlong(s, f))
      )
      .print
      .map(show)
      .evalMap(x => IO { x.foreach(println) })
      .print
      .compile
      .drain

  val run = part2

}
