package nl.woupiestek.equalizer.game

import scalaz._
import scala.annotation.tailrec

final class Reader[S, +A] private (
    private val resume: (Int, S) => Either[Reader[S, A], A]
) {
  def flatMap[A0 >: A, B](
      g: A0 => Reader[S, B]
  ): Reader[S, B] = {
    lazy val result: Reader[S, B] = new Reader(
      (limit, s) =>
        if (limit > 0) {
          resume(limit - 1, s) match {
            case Right(x) => g(x).resume(limit - 1, s)
            case Left(x)  => Left(x.flatMap(g))
          }
        } else {
          Left(result)
        }
    )
    result
  }

  def map[A0 >: A, B](g: A0 => B): Reader[S, B] =
    flatMap((a: A0) => Reader.point(g(a)))

  @tailrec final def exhaust(stackLimit: Int, s0: S): A =
    resume(stackLimit, s0) match {
      case Right(value) => value
      case Left(x)      => x.exhaust(stackLimit, s0)
    }

  def local(f: S => S): Reader[S, A] =
    new Reader[S, A]((i, s) => resume(i, f(s)))
}

object Reader {

  def apply[S, A](f: S => A): Reader[S, A] =
    new Reader((_, s0) => Right(f(s0)))

  def point[S, A](a: => A): Reader[S, A] = Reader(_ => a)

  def get[S]: Reader[S, S] = Reader(s => s)

  def monad[S]: Monad[({ type F[A] = Reader[S, A] })#F] = {
    type F[A] = Reader[S, A]
    new Monad[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
        fa.flatMap(f)
      def point[A](a: => A): F[A] = Reader(_ => a)
    }
  }
}
