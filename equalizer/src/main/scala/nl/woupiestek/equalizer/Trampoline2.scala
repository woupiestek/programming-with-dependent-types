package nl.woupiestek.equalizer
import scala.annotation.tailrec

final case class Trampoline2[+A](
    resume: Int => Either[Trampoline2[A], A]
) {

  final def flatMap[A0 >: A, B](
      g: A0 => Trampoline2[B]
  ): Trampoline2[B] = {
    lazy val result: Trampoline2[B] = Trampoline2(
      limit =>
        if (limit > 0) {
          resume(limit - 1) match {
            case Right(x) => g(x).resume(limit - 1)
            case Left(h)  => Left(h.flatMap(g))
          }
        } else Left(result) // no stack left: do nothing.
    )
    result
  }

  @tailrec final def exhaust(stackLimit: Int): A =
    resume(stackLimit) match {
      case Right(value) => value
      case Left(x)      => x.exhaust(stackLimit)
    }

}

object Trampoline2 {

  def pure[A](a: => A): Trampoline2[A] =
    Trampoline2(_ => Right(a))
  def suspend[A](ta: => Trampoline2[A]): Trampoline2[A] =
    pure(()).flatMap((_: Unit) => ta)

  def tailRecM[A, B](
      a: A
  )(f: A => Trampoline2[Either[A, B]]): Trampoline2[B] = {
    f(a).flatMap[Either[A, B], B] {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
    }
  }

}
