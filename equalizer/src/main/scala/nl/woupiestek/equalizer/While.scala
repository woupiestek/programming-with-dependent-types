package nl.woupiestek.equalizer

import scalaz._

import scala.annotation.tailrec

final case class While[T](next: () => T \/ While[T]) {
  @tailrec def exhaust: T = next() match {
    case -\/(x) => x
    case \/-(x) => x.exhaust
  }
}

object While {

  def cons[T](t: => T \/ While[T]) = While(() => t)

  def ana[A, B](f: A => B \/ A)(a: => A): While[B] = cons(f(a).map(ana(f)(_)))

  def suspend[A](a: => While[A]): While[A] = cons(\/-(a))

  implicit val instance: Monad[While] with BindRec[While] =
    new Monad[While] with BindRec[While] {

      override def point[A](a: => A): While[A] = cons(-\/(a))

      override def bind[A, B](fa: While[A])(f: A => While[B]): While[B] = {
        val a = fa.exhaust
        suspend(f(a))
      }

      @tailrec override def tailrecM[A, B](f: A => While[A \/ B])(a: A): While[B] =
        f(a).exhaust match {
          case -\/(b) => tailrecM(f)(b)
          case \/-(b) => point(b)
        }
    }

}