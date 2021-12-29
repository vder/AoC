package y2021.day22

import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration.*
import y2021.common.*
import cats.implicits.*
import cats.implicits.given
import fs2.Pipe
import fs2.Stream
import scala.annotation.tailrec
import fs2.Pull
import cats.effect.kernel.Sync
import cats.Foldable.ops.toAllFoldableOps
import scala.language.postfixOps
import cats.instances.option.*
import cats.syntax.all

object Main extends IOApp.Simple {

  trait CubeState
  object CubeState {
    def apply(s: String): Option[CubeState] = s match {
      case "on"  => On.some
      case "off" => Off.some
      case _     => None
    }
  }

  case object On extends CubeState
  case object Off extends CubeState

  case class Cube(
      x: (Long, Long),
      y: (Long, Long),
      z: (Long, Long)
  ) {
    def intersect(that: Cube): Option[Cube] = {
      val newX = (math.max(x._1, that.x._1), math.min(x._2, that.x._2))
      val newY = (math.max(y._1, that.y._1), math.min(y._2, that.y._2))
      val newZ = (math.max(z._1, that.z._1), math.min(z._2, that.z._2))
      (newX._1 < newX._2 && newY._1 < newY._2 && newZ._1 < newZ._2)
        .guard[Option]
        .as(Cube(newX, newY, newZ))
    }

    def split(inside: Cube) =
      (Cube((x._1, inside.x._1 - 1), y, z) :: Cube(
        (inside.x._2 + 1, x._2),
        y,
        z
      ) ::
        Cube(inside.x, (y._1, inside.y._1 - 1), z) :: Cube(
          inside.x,
          (inside.y._2 + 1, y._2),
          z
        ) ::
        Cube(inside.x, inside.y, (z._1, inside.z._1 - 1)) :: Cube(
          inside.x,
          inside.y,
          (inside.z._2 + 1, z._2)
        ) :: Nil).filter(cube =>
        cube.x._2 - cube.x._1 >= 0 && cube.y._2 - cube.y._1 >= 0 && cube.z._2 - cube.z._1 >= 0
      )

    def -(that: Cube): List[Cube] =
      intersect(that).fold(List(this))(i => this.split(i))

    def +(that: Cube): List[Cube] =
      intersect(that).fold(List(this, that))(i => this :: (this - i))

    def size: BigInt =
      BigInt(x._2 + 1 - x._1) * BigInt(y._2 + 1 - y._1) * BigInt(
        z._2 + 1 - z._1
      )
  }

  @tailrec
  def addToList(
      input: List[Cube],
      curr: Cube,
      buffer: List[Cube] = List(),
      result: List[Cube] = List()
  ): List[Cube] =
    input match {
      case Nil =>
        buffer match {
          case Nil    => curr :: result
          case h :: t => addToList(curr :: result, h, t, List())
        }
      case h :: tail =>
        h.intersect(curr) match {
          case None => addToList(tail, curr, buffer, h :: result)
          case Some(i) =>
            curr.split(i) match {
              case Nil =>
                buffer match {
                  case Nil => h :: tail ::: result
                  case h1 :: t =>
                    addToList(h :: result, h1, t ::: tail, List())
                }
              case h1 :: t =>
                addToList(tail, h1, t ::: buffer, h :: result)
            }

        }
    }

  @tailrec
  def removeFromList(
      input: List[Cube],
      curr: Cube,
      buffer: List[Cube] = List(),
      result: List[Cube] = List()
  ): List[Cube] =
    input match {
      case Nil =>
        buffer match {
          case Nil    => result
          case h :: t => removeFromList(result, h, t, List())
        }
      case h :: tail =>
        h.intersect(curr) match {
          case None => removeFromList(tail, curr, buffer, h :: result)
          case Some(i) =>
            curr.split(i) match {
              case Nil =>
                buffer match {
                  case Nil => h.split(i) ::: tail ::: result
                  case h1 :: t =>
                    removeFromList(
                      h.split(i) ::: tail ::: result,
                      h1,
                      t,
                      List()
                    )
                }
              case h1 :: t =>
                removeFromList(
                  tail,
                  h1,
                  t ::: buffer,
                  h.split(i) ::: result
                )
            }

        }
    }

  def pf(
      xmin: String,
      xmax: String,
      ymin: String,
      ymax: String,
      zmin: String,
      zmax: String,
      action: String
  ): PartialFunction[(Int, Int, Int), String] = {
    case (x: (Int, Int, Int))
        if x._1 >= xmin.toInt && x._1 <= xmax.toInt && x._2 >= ymin.toInt && x._2 <= ymax.toInt && x._3 >= zmin.toInt && x._3 <= zmax.toInt =>
      action
  }

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day22_test.txt")

  val part1 =
    lines.print
      .collect {
        case s"$action x=$xmin..$xmax,y=$ymin..$ymax,z=$zmin..$zmax" => (
          pf(xmin, xmax, ymin, ymax, zmin, zmax, action)
        )
      }
      .fold(List[PartialFunction[(Int, Int, Int), String]]())((l, e) => e :: l)
      .map(
        _.foldLeft(PartialFunction.empty[(Int, Int, Int), String])((acc, el) =>
          acc.orElse(el)
        )
      )
      .map(f =>
        (for {
          x <- -50 to 50
          y <- -50 to 50
          z <- -50 to 50
          cube = (x, y, z)
          if f.isDefinedAt(cube)
          if f(cube) == "on"
        } yield (1)).size
      )
      .print
      .compile
      .drain

  val part2 =
    lines.print
      .collect { case s"$action x=$xmin..$xmax,y=$ymin..$ymax,z=$zmin..$zmax" =>
        (
          (
            CubeState(action),
            Cube(
              (xmin.toLong, xmax.toLong),
              (ymin.toLong, ymax.toLong),
              (zmin.toLong, zmax.toLong)
            )
          )
        )
      }
      .print
      .fold(List[(Option[CubeState], Cube)]())((l, e) => e :: l)
      .evalTap(x => IO { println(s"size= ${x.size}") })
      .map(_.foldRight(List[Cube]()) {
        case ((Some(On), cube), l)  => addToList(l, cube)
        case ((Some(Off), cube), l) => removeFromList(l, cube)
        case (_, l)                 => l
      })
      .print
      .map(_.foldLeft(BigInt(0))((acc, cube) => acc + cube.size))
      // .map(_.map(_.size).filterNot(_ > 0))
      .print
      .compile
      .drain

  val part3 =
    Stream
      .emit(
        removeFromList(
          List(),
          Cube((0, 10), (0, 10), (0, 10))
        )
      )
      .covary[IO]
      .print
      .map(_.foldLeft(BigInt(0))((acc, e) => acc + e.size))
      .print
      .compile
      .drain

  val run = part2

}
