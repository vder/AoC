package y2021.common

import cats.effect.kernel.Sync
import fs2.Stream
import cats.Monad

extension [F[_]: Sync, A](s: Stream[F, A]) {
  def print = s.evalTap(x => Sync[F].delay(println(x)))
}
