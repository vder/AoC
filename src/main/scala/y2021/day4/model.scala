package y2021.day4

import cats.kernel.Monoid
import cats.syntax.all.toSemigroupKOps

object model {

  case class Board(v: Vector[Vector[Int]]) {

    def combine(a: Vector[Vector[Int]], b: Vector[Vector[Int]]) =
      if (a.isEmpty) b
      else if (b.isEmpty) a
      else a.zip(b).map { case (v1, v2) => v1 ++ v2 }

    given vM: Monoid[Vector[Vector[Int]]] with {
      def empty = Vector[Vector[Int]]()
      def combine(a: Vector[Vector[Int]], b: Vector[Vector[Int]]) =
        if (a.isEmpty) b
        else if (b.isEmpty) a
        else a.zip(b).map { case (v1, v2) => v1 ++ v2 }
    }

    val transpose =
      v.foldLeft(Vector[Vector[Int]]())((r, el) =>
        vM.combine(r,  el.map(Vector(_: Int)))
      //  r <+> el.map(Vector(_: Int))
      )

    val lines = v ++ transpose

    def isWin(s: Set[Int]) = lines.find(v => v.toSet.subsetOf(s)).isDefined

    def unmarked(s: Set[Int]) = v.flatten.filter(i => !s.contains(i))
  }

}
