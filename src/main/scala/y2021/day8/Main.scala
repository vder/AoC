package y2021.day8

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

  val lines /*: Stream[IO, (List[Set[Char]], List[Set[Char]])]*/ =
    FileHandler[IO]
      .readFile("src/main/resources/day8.txt")
      .map(l => {
        val digits :: output :: Nil = l.split("\\|").toList: @unchecked

        (
          digits.split(" ").toList.map(_.trim).map(_.toSet),
          output.split(" ").toList.map(_.trim).map(_.toSet).filterNot(_.isEmpty)
        )
      })

  val part1 = lines
    .flatMap {
      case (d, o) => {
        val decoder = rules(d).runA(Map()).value
        Stream.emits(o.map(decoder(_)))
      }
    }
    .filter(x => x==1 || x==4 ||x==7 || x==8)
    .print
    .compile
    .count
    .flatMap(x=> IO{println(x)})
    .void


    val part2 = lines
    .map{
      case (d, o) => {
        val decoder = rules(d).runA(Map()).value
        o.map(decoder).mkString.toInt
      }
    }
    .fold(0)(_+_)
    .print
    .compile
    .drain
    

  type Display = Map[Int, Set[Char]]

  def rules(digits: List[Set[Char]]) =
    for {
      _ <- State.set[Display](Map(1 -> digits.filter(_.size == 2).head))
      _ <- State.modify[Display](s =>
        s + (4 -> digits.filter(_.size == 4).head)
      )
      _ <- State.modify[Display](s =>
        s + (7 -> digits.filter(_.size == 3).head)
      )
      _ <- State.modify[Display](s =>
        s + (8 -> digits.filter(_.size == 7).head)
      )
      _ <- State.modify[Display](s =>
        s + (9 -> digits.filter(_.size == 6).filter(s(4).subsetOf(_)).head)
      )
      _ <- State.modify[Display](s =>
        s + (2 -> digits
          .filter(_.size == 5)
          .filter((s(8) -- s(9)).subsetOf(_))
          .head)
      )
      _ <- State.modify[Display](s =>
        s + (5 -> digits
          .filter(_.size == 5)
          .filter((s(9) -- s(2)).subsetOf(_))
          .head)
      )
      _ <- State.modify[Display](s =>
        s + (3 -> digits
          .filter(_.size == 5)
          .filterNot(_ == s(2))
          .filterNot(_ == s(5))
          .head)
      )
      _ <- State.modify[Display](s =>
        s + (6 -> digits
          .filter(_.size == 6)
          .filterNot(_ == s(9))
          .filterNot(s(7).subsetOf(_))
          .head)
      )
      _ <- State.modify[Display](s =>
        s + (0 -> digits
          .filter(_.size == 6)
          .filterNot(_ == s(9))
          .filterNot(_ == s(6))
          .head)
      )
      out <- State.get
    } yield (out.toList.map(_.swap).toMap)

  val run = part2
}
