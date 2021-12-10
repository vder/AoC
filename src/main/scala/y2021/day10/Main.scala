package y2021.day10

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

  @tailrec
  def parse(line: List[Char], state: List[Char]): Option[Char] =
    line.headOption match {
      case Some(c) if List('[', '(', '{', '<').contains(c) =>
        parse(line.tail, c :: state)
      case Some(c) if state.headOption == c.opposite.some =>
        parse(line.tail, state.tail)
      case None    => None
      case Some(c) => c.some
    }
  @tailrec
  def parse2(line: List[Char], state: List[Char]): List[Char] =
    line.headOption match {
      case Some(c) if List('[', '(', '{', '<').contains(c) =>
        parse2(line.tail, c :: state)
      case Some(c) if state.headOption == c.opposite.some =>
        parse2(line.tail, state.tail)
      case None    => state.map(_.opposite)
      case Some(c) => List()
    }

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day10.txt")

  val part1Scoring: PartialFunction[Char, Int] = {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  val part1 = lines
    .map(x => (x, parse(x.toList, List())))
    .collect { case (x, Some(y)) => y }
    .collect { part1Scoring }
    .fold(0)(_ + _)
    .print
    .compile
    .drain


  val part2Scoring: PartialFunction[Char, Int] = {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }
  

  val part2 = lines
    .map(x => (x, parse2(x.toList, List())))
    .collect { case (x, l) if !l.isEmpty => l }
    .map(_.foldLeft(0L)((score,el) =>5*score + part2Scoring(el) ))
    .print
    .compile
    .toVector
    .map(v => v.sorted.apply((v.size-1)/2))
    .flatMap(x=> IO{println(x)})
    .void

  val run = part2

  extension (c: Char) {
    def opposite: Char = c match {
      case '(' => ')'
      case ')' => '('
      case '[' => ']'
      case ']' => '['
      case '{' => '}'
      case '}' => '{'
      case '<' => '>'
      case '>' => '<'
    }
  }
}
