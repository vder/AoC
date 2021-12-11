package y2021.day11

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

  type EnergyMap = Map[(Int, Int), Int]
  type Flashed = Set[(Int, Int)]
  type EnergyState = (EnergyMap, Int)

  val toMap: Vector[Vector[Int]] => Map[(Int, Int), Int] =
    v =>
      v.zipWithIndex.flatMap { case (v, i) =>
        v.zipWithIndex.map { case (v, j) => ((i, j) -> v) }
      }.toMap

  @tailrec
  def flash(energyState: EnergyState, f: Flashed): EnergyState = {
    val (m, counter) = energyState
    val readyToFlash =
      m.filter { case (_, v) => v > 9 }.keySet.filterNot(f.contains)
    if (readyToFlash.isEmpty) (m, counter + f.size)
    else {
      val adjacents = for {
        (x, y) <- readyToFlash.toList
        a <- -1 to 1
        b <- -1 to 1
        if a != 0 || b != 0
        if m.contains((x + a, y + b))
      } yield (x + a, y + b)

      val delta =
        adjacents
          .groupMapReduce(identity)(_ => 1)(_ + _)

      flash(
        (
          m ++ delta.map((k, v) => (k -> (m(k) + v))),
          counter
        ),
        f ++ readyToFlash
      )
    }
  }

  val increaseEnergy =
    State.modify[EnergyState](s => (s._1.view.mapValues(_ + 1).toMap, s._2))
  val flashAll = State.modify[EnergyState](s => flash(s, Set()))
  val reset = State.modify[EnergyState](s =>
    (s._1 ++ s._1.collect { case (k, v) if v > 9 => (k -> 0) }, s._2)
  )

  val oneStep = for {
    _ <- increaseEnergy
    _ <- flashAll
    _ <- reset
  } yield ()

  def iterate(n: Int): State[EnergyState, Unit] =
    if (n <= 0) State.empty else oneStep >> iterate(n - 1)

  def untilAllFlash(n: Int): State[EnergyState, Int] =
    oneStep
      .inspect((m, cnt) => m.values.toList.forall(_ == 0))
      .flatMap(cond => if (cond) State.pure(n) else untilAllFlash(n + 1))

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day11.txt")
      .map(_.toVector.map(_.asDigit))
      .fold(Vector[Vector[Int]]())((r, l) => r :+ l)
      .map(toMap)

  val part1 =
    lines.map(m => iterate(100).runS((m, 0)).value).print.compile.drain

  val part2 =
    lines.map(m => untilAllFlash(1).runA((m, 0)).value).print.compile.drain
  val run = part2

}
