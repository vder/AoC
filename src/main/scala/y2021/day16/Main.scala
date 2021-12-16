package y2021.day16

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
import cats.data.NonEmptyList
import cats.parse.Parser0
import y2021.day16.model.Literal
import y2021.day16.model.Operator
import y2021.day16.model.Version
import y2021.day16.model.TypeId

object PackageParser {

  import cats.parse.Parser
  import cats.parse.Rfc5234.*
  import model.*

  val chars2Int: List[Char] => Long =
    x =>
      x.reverse.zipWithIndex.foldLeft(0L) { case (acc, (e, i)) =>
        acc + e.asDigit * scala.math.pow(2, i).toLong
      }

  def bitParser(n: Int) = bit
    .rep(n, n)
    .map(_.toList)
    .map(chars2Int)

  lazy val versionParser = bitParser(3).map(x => Version(x.toInt))
  lazy val literalTypeParser = Parser.string("100").as(TypeId(4))
  lazy val typeParser = bitParser(3).map(x => TypeId(x.toInt))

  lazy val valueParser: Parser0[Long] =
    ((Parser.char('1') *> Parser.anyChar.rep(4, 4)).rep0 ~ (Parser.char(
      '0'
    ) *> Parser.anyChar.rep(4, 4)).rep0(1, 1)).map { case (l1, l2) =>
      chars2Int(l1.map(_.toList).flatten ::: l2.map(_.toList).flatten)
    }

  lazy val literalParser: Parser[Package] =
    (versionParser ~ literalTypeParser ~ valueParser)
      .map { case ((v, t), l) =>
        y2021.day16.model.Literal(v, t, l)
      }

  def lengthType0Operator(p: Parser[Package]): Parser[Package] =
    (versionParser ~ (typeParser <* Parser.char('0')) ~ bitParser(15)
      .flatMap(n => {

        Parser.anyChar
          .rep(n.toInt, n.toInt)
          .map(x =>
            (p.rep0 <* Parser.anyChar.rep0(0, 3))
              .parseAll(x.mkString_("")) match {
              case Right(res) => res
              case Left(e) =>
                throw new RuntimeException(e.toString /*x.mkString_("")*/ )
            }
          )
      }))
      .map { case ((v, t), l) =>
        Operator(v, t, LengthTypeId(false), l)
      }

  def lengthType1Operator(p: Parser[Package]): Parser[Package] =
    (versionParser ~ (typeParser <* Parser.char('1')) ~ bitParser(11).flatMap(
      n => p.rep(n.toInt, n.toInt)
    )).map { case ((v, t), l) =>
      Operator(v, t, LengthTypeId(true), l.toList)
    }

  val packageParser: Parser[Package] = Parser.recursive { p =>
    (literalParser.backtrack | lengthType0Operator(
      p
    ).backtrack | lengthType1Operator(p).backtrack)
  }

  val packageListParser: Parser[List[Package]] =
    packageParser.rep(1).map(_.toList)
}

object Main extends IOApp.Simple {

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day16.txt")
      .map(_.toList.collect {
        case '0' => "0000"
        case '1' => "0001"
        case '2' => "0010"
        case '3' => "0011"
        case '4' => "0100"
        case '5' => "0101"
        case '6' => "0110"
        case '7' => "0111"
        case '8' => "1000"
        case '9' => "1001"
        case 'A' => "1010"
        case 'B' => "1011"
        case 'C' => "1100"
        case 'D' => "1101"
        case 'E' => "1110"
        case 'F' => "1111"
      }.mkString)

  //7981 s solution
  val part1 =
    lines
      .map(PackageParser.packageListParser.parse)
      .collect { case Right(x) =>
        x._2

      }
      .map(x => countVersions(x))
      .print
      .compile
      .drain

  val manual =
    Stream
      .emit("9C0141080250320F1802104A08")
      .covary[IO]
      .map(_.toList.collect {
        case '0' => "0000"
        case '1' => "0001"
        case '2' => "0010"
        case '3' => "0011"
        case '4' => "0100"
        case '5' => "0101"
        case '6' => "0110"
        case '7' => "0111"
        case '8' => "1000"
        case '9' => "1001"
        case 'A' => "1010"
        case 'B' => "1011"
        case 'C' => "1100"
        case 'D' => "1101"
        case 'E' => "1110"
        case 'F' => "1111"
      }.mkString)

  val part2 =
    lines
      .map(PackageParser.packageListParser.parse)
      .collect { case Right(x) =>
        x._2

      }
      .map(x => x.map(calculate(_)))
      .print
      .compile
      .drain

  val run = part2

  def countVersions(li: List[y2021.day16.model.Package]): BigInt =
    li.foldLeft(BigInt(0))((acc, e) =>
      val cnt = e match {
        case Literal(Version(i), _, _) => BigInt(i)
        case Operator(Version(i), _, _, subPackages) =>
          i + countVersions(subPackages)
      }
      acc + cnt
    )

  def calculate(p: y2021.day16.model.Package): BigInt =
    p match {
      case Literal(_, _, value) => BigInt(value)
      case Operator(_, TypeId(0), _, subPackages) =>
        subPackages.map(x => calculate(x)).sum
      case Operator(_, TypeId(1), _, subPackages) =>
        subPackages.map(calculate(_)).product
      case Operator(_, TypeId(2), _, subPackages) =>
        subPackages.map(calculate(_)).min
      case Operator(_, TypeId(3), _, subPackages) =>
        subPackages.map(calculate(_)).max
      case Operator(_, TypeId(5), _, subPackages) => {
        val vals = subPackages.map(calculate(_))
        if (vals(0) > vals(1)) 1 else 0
      }
      case Operator(_, TypeId(6), _, subPackages) => {
        val vals = subPackages.map(calculate(_))
        if (vals(0) < vals(1)) 1 else 0
      }
      case Operator(_, TypeId(7), _, subPackages) => {
        val vals = subPackages.map(calculate(_))
        if (vals(0) == vals(1)) 1 else 0
      }
      case _ => throw new RuntimeException("Zle")
    }

}
