package nl.woupiestek.equalizer

import scalaz._

import scala.annotation.tailrec

final case class While[T](next: () => While[T] \/ T) {
  @tailrec def exhaust: T = next() match {
    case -\/(x) => x.exhaust
    case \/-(x) => x
  }
}

object While {

  def suspend[A](a: => While[A]): While[A] = While(() => -\/(a))

  implicit val instance: Monad[While] with BindRec[While] =
    new Monad[While] with BindRec[While] {

      override def point[A](a: => A): While[A] = While(() => \/-(a))

      @tailrec override def bind[A, B](fa: While[A])(f: A => While[B]): While[B] =
        fa.next() match {
          case -\/(a) => bind(a)(f)
          case \/-(b) => suspend(f(b))
        }

      override def tailrecM[A, B](f: A => While[A \/ B])(a: A): While[B] =
        bind(f(a)) {
          case -\/(b) => suspend(tailrecM[A, B](f)(b))
          case \/-(b) => point[B](b)
        }
    }

}