package y2021.day2

import cats.effect.{IO, IOApp}
import scala.concurrent.duration._
import y2021.common.FileHandler
import cats.implicits._
import fs2.Pipe
import y2021.day2.model._
import y2021.day2.model.Command._

object Main extends IOApp.Simple {

  val toCommands: Pipe[IO, String, Command] =
    _.mapFilter(x => Command.safeApply(x))

  val lines = FileHandler[IO]
    .readFile("src/main/resources/day2_input.txt")
    .through(toCommands)

  val part1Controller: (Part1State, Command) => Part1State =
    (s: Part1State, c: Command) =>
      c match {
        case Up(x)      => Part1State(s.pos, s.depth - x)
        case Down(x)    => Part1State(s.pos, s.depth + x)
        case Forward(x) => Part1State(s.pos + x, s.depth)
      }

  val part2Controller: (Part2State, Command) => Part2State =
    (s: Part2State, c: Command) =>
      c match {
        case Up(x)   => s.copy(aim = s.aim - x)
        case Down(x) => s.copy(aim = s.aim + x)
        case Forward(x) =>
          s.copy(pos = s.pos + x, depth = s.depth + s.aim * x)
      }

  val run =
    lines
      .fold(Part1State(0, 0))(part1Controller)
      .evalMap(x => IO { println(x) })
      .compile
      .drain
    *>
    lines
      .fold(Part2State(0, 0, 0))(part2Controller)
      .evalMap(x => IO { println(x) })
      .compile
      .drain
      
}
