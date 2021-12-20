package y2021.day19

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
import scala.collection.mutable.Queue
import scala.collection.mutable.Set

object Main extends IOApp.Simple {

  val orient =
    List(
      Vector((0, 1), (1, 1), (2, 1)),
      Vector((2, 1), (1, 1), (0, -1)),
      Vector((2, -1), (1, 1), (0, 1)),
      Vector((0, -1), (1, 1), (2, -1)),
      Vector((2, 1), (0, 1), (1, 1)),
      Vector((1, -1), (0, 1), (2, 1)),
      Vector((2, -1), (0, 1), (1, -1)),
      Vector((1, 1), (0, 1), (2, -1)),
      Vector((1, 1), (2, 1), (0, 1)),
      Vector((0, -1), (2, 1), (1, 1)),
      Vector((1, -1), (2, 1), (0, -1)),
      Vector((0, 1), (2, 1), (1, -1)),
      Vector((0, 1), (1, -1), (2, -1)),
      Vector((2, 1), (1, -1), (0, 1)),
      Vector((0, -1), (1, -1), (2, 1)),
      Vector((2, -1), (1, -1), (0, -1)), ///XXXX
      Vector((1, 1), (0, -1), (2, 1)),
      Vector((2, -1), (0, -1), (1, 1)),
      Vector((1, -1), (0, -1), (2, -1)),
      Vector((2, 1), (0, -1), (1, -1)),
      Vector((1, 1), (2, -1), (0, -1)),
      Vector((0, 1), (2, -1), (1, 1)),
      Vector((1, -1), (2, -1), (0, 1)),
      Vector((0, -1), (2, -1), (1, -1))
    )

  type Readings = Array[Array[Int]]
  type Point = (Int, Int, Int)

  def recalculate(r: Readings, p: Point) = {
    println(p)
    for {
      j <- 0 until r(0).size
    } {
      r(0)(j) = r(0)(j) + p._1
      r(1)(j) = r(1)(j) + p._2
      r(2)(j) = r(2)(j) + p._3
      // println(s"${r(0)(j)}, ${r(1)(j)}, ${r(2)(j)} ")
    }
    r
  }

  def adjacentWithOrientation(
      s1: Readings,
      s2: Readings,
      toCheck: List[Vector[(Int, Int)]]
  ): Option[(Readings, Point)] =
    if (toCheck.isEmpty) None
    else {
      val s2Reordered = generateOrientation(s2, toCheck.head)
      val point = adjacent(s1, s2Reordered, 12)
      if (point.isEmpty)
        adjacentWithOrientation(s1, s2, toCheck.tail)
      else
        point.map(x => (recalculate(s2Reordered, x), x))

    }

  def adjacent(
      s1: Readings,
      s2: Readings,
      bound: Int = 12
  ): Option[Point] = {

    val diff = Array.ofDim[(Int, Int, Int)](s1(0).size, s2(0).size)
    for {
      i <- 0 until s1(0).size
      j <- 0 until s2(0).size
    }(diff(i)(j) =
      (s1(0)(i) - s2(0)(j), s1(1)(i) - s2(1)(j), s1(2)(i) - s2(2)(j)))

    diff
      .flatMap(identity)
      .groupBy(identity)
      .map((k, a) => (k, a.size))
      .filter(_._2 >= bound)
      .map(_._1)
      .headOption

  }

  def generateOrientation(s: Readings, dir: Vector[(Int, Int)]): Readings =
    Array(
      s(dir(0)._1).map(x => x * dir(0)._2),
      s(dir(1)._1).map(x => x * dir(1)._2),
      s(dir(2)._1).map(x => x * dir(2)._2)
    )

  def findPoints(
      acc: List[(Readings, Point)],
      toCheck: Queue[Readings]
  ): List[(Readings, Point)] =
    if (toCheck.isEmpty) acc
    else {

      println(toCheck.size)
      val toAssign = toCheck.dequeue

      val found = acc.iterator
        .map { case (r, p) => adjacentWithOrientation(r, toAssign, orient) }
        .dropWhile(_.isEmpty)
        .nextOption
        .flatten
      println("start")
      toAssign.transpose.foreach(x => println(x.mkString(",")))
      println("end")
      if (found.isDefined)
        findPoints(found.get :: acc, toCheck)
      else
        findPoints(acc, toCheck.enqueue(toAssign))

    }

  val lines =
    FileHandler[IO]
      .readFile("src/main/resources/day19.txt")
      .fold(List[Vector[Vector[Int]]]())((list, line) =>
        if (line.contains("scanner")) Vector() :: list
        else if (!line.isBlank)
          (list.head :+ line.split(",").toVector.map(_.toInt)) :: list.tail
        else list
      )
      .map(l => l.map(_.transpose.toVector).reverse.toVector)
      .map(l => l.map(v => v.map(_.toArray).toArray))

  val part1 = lines
    .flatMap(v =>
      Stream.emits(findPoints(List((v(0), (0, 0, 0))), Queue.from(v.tail)))
    )
    .flatMap(x => Stream.emits(x._1.transpose))
    .map(x => (x(0), x(1), x(2)))
    .fold(Set[(Int, Int, Int)]())((s, e) => { s.add(e); s })
    .map(_.size)
    .print
    .compile
    .drain

  val part2 = lines
    .flatMap(v =>
      Stream.emits(findPoints(List((v(0), (0, 0, 0))), Queue.from(v.tail)))
    )
    .map(x => x._2)
    .fold(List[(Int,Int,Int)]())((l,e)=> e::l)
    .map(l => (for {
       p1 <-l
       p2 <- l
       if p1 != p2
    } yield(  math.abs(p1._1 - p2._1)+(math.abs(p1._2 - p2._2) + math.abs(p1._3 - p2._3)))).max
    
    )
    .print
    .compile
    .drain

  val run = part2

}
