package y2021.day2

import cats.effect.{IO, IOApp}
import scala.concurrent.duration._
import y2021.common.FileHandler
import cats.implicits._
import fs2.Pipe
import fs2.Stream
import y2021.day2.model._
import y2021.day2.model.Command._
import cats.data.State

object MainStateMonad extends IOApp.Simple {

  val toCommands: Pipe[IO, String, Command] =
    _.mapFilter(x => Command.safeApply(x))

  val lines: Stream[IO, Command] = FileHandler[IO]
    .readFile("src/main/resources/day2_input.txt")
    .through(toCommands)

  def part1Controler(c: Command): State[Part1State, Int] = State { s =>
    {
      val newState = c match {
        case Up(x)      => Part1State(s.pos, s.depth - x)
        case Down(x)    => Part1State(s.pos, s.depth + x)
        case Forward(x) => Part1State(s.pos + x, s.depth)
      }
      (newState, newState.pos * newState.depth)
    }
  }

  def part2Controler(c: Command): State[Part2State, Int] = State { s =>
    {
      val newState = c match {
        case Up(x)   => s.copy(aim = s.aim - x)
        case Down(x) => s.copy(aim = s.aim + x)
        case Forward(x) =>
          s.copy(pos = s.pos + x, depth = s.depth + s.aim * x)
      }
      (newState, newState.pos * newState.depth)
    }
  }

  def calculate[S](controler: Command => State[S, Int])(initial: S)(
      commands: Stream[IO, Command]
  ): Stream[IO, Unit] =
    commands
      .map(controler)
      .reduce((c1, c2) => c1.flatMap(_ => c2))
      .evalMap(x => IO.eval(x.run(initial)))
      .foreach(x => IO { println(x) })

  val run =
    (calculate(part1Controler _)(Part1State(0, 0))(lines)
      ++ calculate(part2Controler _)(Part2State(0, 0, 0))(
        lines
      )).compile.drain
}
