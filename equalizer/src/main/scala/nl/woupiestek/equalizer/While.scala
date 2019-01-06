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

  implicit object Instance extends Monad[While] with BindRec[While] {

    override def point[A](a: => A): While[A] = While(() => \/-(a))

    override def bind[A, B](fa: While[A])(f: A => While[B]): While[B] =
      fa.next().fold(tail(bind(_)(f)), f)

    override def tailrecM[A, B](f: A => While[A \/ B])(a: A): While[B] =
      bind(f(a))(_.fold(tail(tailrecM[A,B](f)), point[B](_)))

    private def tail[A, B](f: A => While[B])(a: A): While[B] = suspend(f(a))
  }

}