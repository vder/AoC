package y2021.day20

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
import cats.Foldable.ops.toAllFoldableOps
import scala.language.postfixOps

object Main extends IOApp.Simple {

  type Image = Set[(Int, Int)]
  type Point = (Int, Int)
  type Algorithm = Vector[Char]

  def image2Map(v: Vector[Vector[Char]]): Image =
    (for {
      i <- 0 until v.size
      j <- 0 until v(i).size
      if v(i)(j) == '#'
    } yield ((i, j))).toSet

  def getNewDims(m: Image): (Point, Point) = {
    val (x, y) = m.unzip
    ((x.min, x.max), (y.min, y.max))
  }

  def applyNAlgorithm(m: Image, enc: Algorithm, n: Int): Image =
    if (n <= 0) m
    else {
      val newM = applyAlgorithm(m, enc, n % 2)
      applyNAlgorithm(newM, enc, n - 1)
    }

  def applyAlgorithm(m: Image, enc: Algorithm, default: Int): Image = {

    val ((xmin, xmax), (ymin, ymax)) = getNewDims(m)

    def applyMask(p: Point) = {

      var acc = 0
      var idx = 0
      for {
        i <- p._1 + 1 to p._1 - 1 by -1
        j <- p._2 + 1 to p._2 - 1 by -1
      } {
        acc = acc + scala.math.pow(2, idx).toInt *
          (if (i < xmin || i > xmax || j < ymin || j > ymax) default
           else if (m.contains((i, j))) 1
           else 0)
        idx = idx + 1
      }

      enc(acc)

    }

    val newSet = scala.collection.mutable.Set[(Int, Int)]()

    for {
      i <- xmin - 2 to xmax + 2
      j <- ymin - 2 to ymax + 2
      newVal = applyMask((i, j))
      if newVal == '#'
    }
    {
      newSet.add((i, j))
    }

    newSet.toSet

  }

  def printImage(m: Image): Unit = {

    val ((xmin, xmax), (ymin, ymax)) = getNewDims(m)
    println(s"($xmin, $xmax), ($ymin, $ymax)")

    val arr = Array.fill[Char](xmax - xmin, ymax - ymin)('0')
    for {
      i <- xmin - xmin until xmax - xmin
      j <- ymin - ymin until ymax - ymin
    } {

      arr(i)(j) = if (m.contains((i + xmin, j + ymin))) '1' else '0'
    }

    arr.map(_.mkString("")).foreach(println)
  }

  val lines =
    (
      FileHandler[IO]
        .readFile("src/main/resources/day20.txt")
        .take(1)
        .map(_.toVector),
      FileHandler[IO]
        .readFile("src/main/resources/day20.txt")
        .drop(1)
        .filterNot(_.isBlank)
        .map(_.toVector)
        .fold(Vector[Vector[Char]]())((v, l) => v :+ l)
        .map(image2Map(_))
    ).tupled

  val part1 =
    lines.print
      .map { case (a, m) => applyNAlgorithm(m, a, 50) }
      // .evalTap(m =>
      //   IO {
      //     println("--------")
      //     printImage(m)
      //     println("-----------")
      //   }
      // )
      .map(_.size)
      .print
      .compile
      .drain
  val run = part1

}
