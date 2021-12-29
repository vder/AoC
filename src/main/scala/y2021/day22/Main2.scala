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
import scala.collection.immutable.NumericRange.Inclusive


object Main2 extends IOApp.Simple {

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
      x: Inclusive[Long],
      y: Inclusive[Long],
      z: Inclusive[Long]
  ) {
    def intersect(that: Cube): Option[Cube] = {
      val newX = math.max(x.start, that.x.start) to math.min(x.end, that.x.end)
      val newY = math.max(y.start, that.y.start) to math.min(y.end, that.y.end)
      val newZ = math.max(z.start, that.z.start) to math.min(z.end, that.z.end)
      (!newX.isEmpty && !newY.isEmpty && !newZ.isEmpty)
        .guard[Option]
        .as(Cube(newX, newY, newZ))
    }

    def split(inside: Cube) =
      (Cube(x.start to inside.x.start - 1, y, z) :: Cube(
        inside.x.end + 1 to x.end,
        y,
        z
      ) ::
        Cube(inside.x, y.start to inside.y.start - 1, z) :: Cube(
          inside.x,
          inside.y.end + 1 to y.end,
          z
        ) ::
        Cube(inside.x, inside.y, z.start to inside.z.start - 1) :: Cube(
          inside.x,
          inside.y,
          inside.z.end + 1 to z.end
        ) :: Nil).filterNot(cube =>
        cube.x.isEmpty || cube.y.isEmpty || cube.z.isEmpty
      )

    def size: BigInt =
      BigInt(x.size) * BigInt(y.size) * BigInt(
        z.size
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
                  case Nil => h :: tail:::result
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
      .readFile("src/main/resources/day22.txt")

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
    lines
      .collect { case s"$action x=$xmin..$xmax,y=$ymin..$ymax,z=$zmin..$zmax" =>
        (
          (
            CubeState(action),
            Cube(
              xmin.toLong to xmax.toLong,
              ymin.toLong to ymax.toLong,
              zmin.toLong to zmax.toLong
            )
          )
        )
      }
      .fold(List[Cube]()) {
        case (l,(Some(On), cube))  => addToList(l, cube)
        case (l,(Some(Off), cube)) => removeFromList(l, cube)
        case (l,_)                 => l
      }
      .map(_.foldLeft(BigInt(0))((acc, cube) => acc + cube.size))
      .print
      .compile
      .drain



  val run = part2

}
