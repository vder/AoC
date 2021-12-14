package y2021.day14

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

  case class Step(pairs: Map[String, Long], counts: Map[String, Long])
  val generateNext: Pipe[IO, (String, Map[String, String]), String] =
    s =>
      s.flatMap { case (init, rules) =>
        Stream.unfold(init)(s => {
          val output = s"${s(0)}${s
            .sliding(2)
            .map(x => s"${rules(x)}${x(1)}")
            .mkString}"

          (output, output).some
        })
      }

  def genNewPair(rules: Map[String, String])(e: String) =
    List(s"${e(0)}${rules(e)}", s"${rules(e)}${e(1)}")

  def nextStep(rules: Map[String, String]): Step => Step = {
    case Step(pairs, counts) => {
      val (newPairs, newCounts) =
        pairs.foldLeft((Map[String, Long](), counts)) {
          case ((pairCount, charCount), (pair, cnt)) => {
            println(pairCount)
            (
              pairCount ++ genNewPair(rules)(pair)
                .map(newE => (newE, cnt + pairCount.getOrElse(newE, 0L)))
                .toMap,
              charCount + (rules(pair) -> (charCount.getOrElse(
                rules(pair),
                0L
              ) + cnt))
            )
          }
        }
      Step(newPairs, newCounts)
    }
  }

  def iterateSteps(n: Int)(rules: Map[String, String]): Step => Step =
    s =>
      if (n <= 0) s
      else iterateSteps(n - 1)(rules)(nextStep(rules)(s))

  def generate(
      rules: Map[String, String]
  ): Pipe[IO, String, String] = {
    def go(
        s: Stream[IO, String],
        prev: Option[String]
    ): Pull[IO, String, Unit] = {
      s.pull.uncons1.flatMap {
        case Some(char, rest) =>
          if (prev.isEmpty)
            go(rest, char.some)
          else
            Pull.output1(prev.get) >> Pull.output1(
              rules(prev.get + char)
            ) >> go(
              rest,
              char.some
            )
        case None =>
          Pull.output1(prev.get) >> Pull.done
      }
    }
    s => go(s, None).stream
  }

  def iterate(n: Int, r: Map[String, String]): Pipe[IO, String, String] =
    s =>
      if (n <= 0) s
      else s.through(generate(r)).through(iterate(n - 1, r))

  val lines =
    (
      FileHandler[IO]
        .readFile("src/main/resources/day14.txt")
        .take(1),
      FileHandler[IO]
        .readFile("src/main/resources/day14.txt")
        .drop(1)
        .collect { case s"$x -> $y" =>
          (x, y)
        }
        .fold(Map[String, String]())((m, e) => m + e)
    ).tupled

  val part1 =
    lines.print
      .through(generateNext)
      .take(10)
      .last
      .collect { case Some(s) => s.groupMapReduce(identity)(_ => 1L)(_ + _) }
      .map(m =>
        val list = m.toList.map(_._2)
        list.max
          - list.min
      )
      .print
      .compile
      .drain

  val part1a =
    lines.print
      .map { case (init, rules) =>
        (
          Step(
            init.sliding(2).toList.groupMapReduce(identity)(_ => 1L)(_ + _),
            init.toList.groupMapReduce(_.toString)(_ => 1L)(_ + _)
          ),
          rules
        )
      }
      .print
      .map { case (s, r) => iterateSteps(40)(r)(s) }
      .map { case Step(_, m) =>
        val list = m.toList.map(_._2)
        list.max
          - list.min
      }
      .print
      .compile
      .drain

  val part2 =
    lines.print
      .flatMap { case (init, rules) =>
        Stream.emits(init.toList.map(_.toString)).through(iterate(40, rules))
      }
      // .fold(Map[String, Int]())((acc, e) => acc + (e ->(acc.getOrElse(e,0) + 1)))
      // .map(m =>
      //   val list = m.toList.map(_._2)
      //   list.max
      //     - list.min
      // )
      .printlns
      .compile
      .drain

  val run = part1a

}
