package y2021.day5

import cats.kernel.Monoid
import cats.syntax.all.toSemigroupKOps

object model {

  case class Point(x: Int, y: Int)

  object Point {
    def makeLine(p1: Point, p2: Point): List[Point] =
      if (p1.x == p2.x)
        (p1.y to p2.y by (p2.y - p1.y).sign).toList.map(Point(p1.x, _))
      else if (p1.y == p2.y)
        (p1.x to p2.x by (p2.x - p1.x).sign).toList.map(Point(_, p1.y))
      else if (Math.abs(p2.y - p1.y) == Math.abs(p2.x - p1.x)) {
        val dir = ((p2.x - p1.x).sign, (p2.y - p1.y).sign)
        (p1.x to p2.x by dir._1).toList.zipWithIndex.map { case (x, i) =>
          Point(x, p1.y + i * dir._2)
        }
      } else List()
  }
}
