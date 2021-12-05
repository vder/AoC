import cats.kernel.Monoid
import fs2.Chunk
import fs2.text
import cats.effect.IO
import fs2.io.file.{Files, Path}
import cats.effect.unsafe.implicits.global
import cats.effect.Sync

val z = Files[IO]
  .readAll(Path("src/main/resources/day1_input.txt"))
  .through(text.utf8.decode[IO])
  .through(text.lines)
  .map(x => x.toInt)

val buf = z.buffer(3)

val io = z
  .sliding(3)
  .map(x => x(0) + x(1) + x(2))
  .sliding(2)
  .map(x => x(1) - x(0))
  .filter(_ > 0)
  .fold(0)((z, el) => z + 1)

io.compile.toList.unsafeRunSync()

IO { println("test") }.unsafeRunSync()

val v1 = Vector(1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0)
val v2 = Vector(1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0)

val v3 = (v1 zip v2)
v3.map { case (x, y) => x + y }

val s = " 3 15  0  2 22"

val v4 = Vector(1, 2, 3, 4, 5, 6)
val v5 = v4.foldLeft(Vector[Vector[Int]]())((r, el) => r :+ Vector(el))

import y2021.day4.model._

val b:Board = Board(Vector(Vector(1, 2), Vector(3, 4)))


given vMonoid: Monoid[Vector[Vector[Int]]] with {
  def empty = Vector[Vector[Int]]()
  def combine(a: Vector[Vector[Int]], b: Vector[Vector[Int]]) =
    if (a.isEmpty) b
    else if (b.isEmpty) a
    else a.zip(b).map { case (v1, v2) => v1 ++ v2 }
}

val b1 = b.v(0).map(Vector(_))
val b2 = b.v(1).map(Vector(_))

b.transpose

vMonoid.combine(vMonoid.empty, b1)

b.v.foldLeft(Vector[Vector[Int]]())((r, el) =>
  vMonoid.combine(r, el.map(Vector(_: Int)))
)




fs2.Stream.emits("1,2,3".split(',').toList).evalTap(x=> IO{println(x)}).compile.drain.unsafeRunSync()



Set(1,2) == Set(2,1)


b.isWin(Set(1))