package nl.woupiestek.equalizer
import nl.woupiestek.equalizer.Trampoline._
import scala.annotation.tailrec

sealed abstract class Trampoline[+A] {

  def flatMap[A0 >: A, B](
      f: A0 => Trampoline[B]
  ): Trampoline[B]

  @tailrec final def run(stackLimit: Int): A = {

    def helper[B](
        ta: Trampoline[B],
        limit: Int
    ): Trampoline[B] =
      ta match {
        case FlatMap(a, b) if (limit > 0) => {
          helper(a, limit - 1) match {
            case Pure(c) =>
              helper(b(c), limit - 1) //need no TCO
            case c => c.flatMap(b)
          }
        }
        case _ => ta
      }

    this match {
      case Pure(a) => a
      case _       => helper(this, stackLimit).run(stackLimit)
    }
  }
}

object Trampoline {

  final case class Pure[+A](a: A) extends Trampoline[A] {
    def flatMap[A0 >: A, B](
        f: A0 => Trampoline[B]
    ): Trampoline[B] = f(a)
  }
  final case class FlatMap[Z, +A](
      a: Trampoline[Z],
      b: Z => Trampoline[A]
  ) extends Trampoline[A] {
    def flatMap[A0 >: A, B](
        f: A0 => Trampoline[B]
    ): Trampoline[B] =
      copy(b = b(_: Z).flatMap(f))
  }

  def suspend[A](a: => Trampoline[A]) =
    FlatMap(Pure(()), (_: Unit) => a)

  def tailRecM[A, B](
      a: A
  )(f: A => Trampoline[Either[A, B]]): Trampoline[B] = {
    f(a).flatMap[Either[A, B], B] {
      case Left(a)  => suspend(tailRecM(a)(f))
      case Right(b) => Pure(b)
    }
  }
}
