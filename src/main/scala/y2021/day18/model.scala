package y2021.day18

import cats.kernel.Monoid
import cats.kernel.Semigroup
import scala.annotation.tailrec
import cats.parse.Parser
import cats.parse.Rfc5234.*
import cats.implicits.*

object model {

  trait SnailNumber {
    def split: (SnailNumber, Boolean) =
      this match {
        case Simple(x) if x >= 10 =>
          (Complex(Simple(x / 2), Simple((x + 1) / 2)), true)
        case Simple(x) => (this, false)
        case Complex(l, r) => {
          val (newL, splitted) = l.split
          if (splitted) (Complex(newL, r), true)
          else {
            val (newR, splitted) = r.split
            (Complex(l, newR), splitted)
          }
        }

      }

    def magnitude: Int =
      this match {
        case Simple(x)     => x
        case Complex(l, r) => 3 * l.magnitude + 2 * r.magnitude
      }

    def addLeft(i: Int): SnailNumber =
      this match {
        case Complex(l, r) => Complex(l.addLeft(i), r)
        case Simple(x)     => Simple(x + i)
      }

    def addRight(i: Int): SnailNumber =
      this match {
        case Complex(l, r) => Complex(l, r.addRight(i))
        case Simple(x)     => Simple(x + i)
      }

    def explodeOnce(deep: Int = 0): (SnailNumber, Option[Int], Option[Int]) = {
      if (deep >= 4) {
        this match {
          case Simple(_)                     => (this, None, None)
          case Complex(Simple(x), Simple(y)) => (Simple(0), x.some, y.some)
        }
      } else {
        this match {
          case Simple(_) => (this, None, None)
          case Complex(l, r) => {
            val (newL, lExp, rExp) = l.explodeOnce(deep + 1)
            if (rExp.isDefined)
              (Complex(newL, r.addLeft(rExp.get)), lExp, 0.some)
            else if (lExp.isDefined) (Complex(newL, r), lExp, 0.some)
            else {
              val (newR, lExp, rExp) = r.explodeOnce(deep + 1)
              if (lExp.isDefined)
                (Complex(newL.addRight(lExp.get), newR), 0.some, rExp)
              else
                (Complex(newL, newR), None, rExp)
            }
          }
        }

      }
    }

    def reduce: SnailNumber = {
      val (result, lExp, rExp) = this.explodeOnce(0)
      if (this != result) result.reduce
      else {
        val (result, splitted) = this.split
        if (splitted) result.reduce
        else this
      }
    }
  }

  case class Complex(left: SnailNumber, right: SnailNumber) extends SnailNumber
  case class Simple(value: Int) extends SnailNumber

  object Complex {
    given Semigroup[Complex] with {
      def combine(a: Complex, b: Complex) =
        Complex(a, b).reduce.asInstanceOf[Complex]
    }
  }

  object SnailNumber {
    def apply(s: String): Option[SnailNumber] =
      SnailNumberParser.complexParser.parseAll(s).toOption.map(_.reduce)

    given Semigroup[SnailNumber] with {
      def combine(a: SnailNumber, b: SnailNumber) =
        Complex(a, b).reduce
    }
  }

  object SnailNumberParser {

    val simpleParser: Parser[SnailNumber] =
      digit.map(_.asDigit).map(Simple.apply)

    val complexParser: Parser[SnailNumber] = Parser.recursive(p =>
      val valueParser = Parser.oneOf(simpleParser :: p :: Nil)

      ((Parser.char('[') *> valueParser <* Parser.char(
        ','
      )) ~ (valueParser <* Parser.char(']'))).map { case (l, r) =>
        Complex(l, r)
      }
    )

  }
}
