package template.y2021.common

import fs2.io.file.Files
import cats.effect.kernel.Async
import fs2.io.file.Path
import fs2.text

trait FileHandler[F[_]] {
  def readFile(path: String): fs2.Stream[F, String]
}

final class LiveFileHandler[F[_]: Async] extends FileHandler[F] {

  override def readFile(path: String): fs2.Stream[F, String] = {
    Files[F]
      .readAll(Path("src/main/resources/day1_input.txt"))
      .through(text.utf8.decode[F])
      .through(text.lines)
  }

}

object FileHandler {
  def apply[F[_]: Async] = LiveFileHandler[F]
}
