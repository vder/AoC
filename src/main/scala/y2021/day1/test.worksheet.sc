import fs2.Chunk
import fs2.text
import cats.effect.IO
import fs2.io.file.{Files, Path}
import cats.effect.unsafe.implicits.global
import cats.effect.Sync
import cats.implicits._

val z = Files[IO]
  .readAll(Path("src/main/resources/day1_input.txt"))
  .through(text.utf8.decode[IO])
  .through(text.lines).map(x => x.toInt)


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


val v1= Vector(1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0)
val v2= Vector(1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0)

val v3 = (v1 zip v2)
v3.map{ case(x,y)=> x+y}
