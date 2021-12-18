package y2021.day18

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
import y2021.day18.model.*
import cats.implicits.*
import cats.implicits.given
import cats.kernel.Semigroup

object Main extends IOApp.Simple {

  //7981 s solution
  val manual =
    Stream
      .emits(
        List(
          // SnailNumber("[[[[4,3],4],4],[7,[[8,4],9]]]").get,
          // SnailNumber("[1,1]").get
          SnailNumber("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]").get
        )
      )
      .covary[IO]
      .print
      .map(_.reduce)
      .print
      .map(_.reduce)
      .print
      // .reduce((a, b) => {
      //   println(s"suming  $a + $b = ${a |+| b}")
      //   a |+| b
      // })
      .print
      .compile
      .drain

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day18.txt")

  val part1 = lines
    .map(SnailNumber.apply)
    .collect { case Some(x) =>
      x.asInstanceOf[Complex]
    }
    .reduce((a, b) => {
      a |+| b
    })
    .print
    .map(_.magnitude)
    .print
    .compile
    .drain

  val part2 = lines
    .map(SnailNumber.apply)
    .collect { case Some(x) =>
      x.asInstanceOf[Complex]
    }
    .foldMap(List(_))
    .flatMap(x => Stream.emits(x.combinations(2).toList))
    .collect { case h :: t :: Nil => h |+| t }
    .map(_.magnitude)
    .print
    .fold(0)((acc, e) => if (e > acc) e else acc)
    .print
    .compile
    .drain

  val run = part2

}


