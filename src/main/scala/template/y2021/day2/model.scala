package template.y2021.day2

import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.Parser

object model {
  enum Command {
    case Up(value: Int)
    case Down(value: Int)
    case Forward(value: Int)
  }

  case class Part1State(pos: Int, depth: Int)
  case class Part2State(aim: Int, pos: Int, depth: Int)

  object Command {
    def safeApply(s: String): Option[Command] =
      s match {
        case s"forward $x" => x.toIntOption.map(Forward(_))
        case s"up $x"      => x.toIntOption.map(Up(_))
        case s"down $x"    => x.toIntOption.map(Down(_))
        case _             => None
      }
  }
}
