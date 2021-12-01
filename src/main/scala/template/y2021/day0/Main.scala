package y2021.day0

import cats.effect.{IO, IOApp}
import scala.concurrent.duration._

object Main extends IOApp.Simple {
  val run =
    for {
      ctr <- IO.ref(0)

      wait = IO.sleep(1.second)
      poll = wait *> ctr.get

      f0 <- poll
        .flatMap(x => IO.println(s"${Thread.currentThread.getName} - ${x} "))
        .foreverM
        .start
      f1 <- poll
        .map(_ % 3 == 0)
        .ifM(IO.println(s"${Thread.currentThread.getName} - fizz"), IO.unit)
        .foreverM
        .start
      f2 <- poll
        .map(_ % 5 == 0)
        .ifM(IO.println(s"${Thread.currentThread.getName} - buzz"), IO.unit)
        .foreverM
        .start

      f3 <- (wait *> ctr.update(_ + 1)).foreverM.start
      _ <- (IO.sleep(
        10.seconds
      ) *> f0.cancel *> f3.cancel *> f2.cancel *> f1.cancel).start
      _ <- f0.join
      _ <- f1.join
      _ <- f2.join
      _ <- f3.join
    } yield ()
}
