package nl.woupiestek.equalizer.parsing

import scalaz._
import scala.annotation.tailrec

final class View[+A](
    private val resume: Int => List[Either[View[A], A]]
) {

  def flatMap[A0 >: A, B](g: A0 => View[B]): View[B] = {
    lazy val result: View[B] = new View(
      limit =>
        if (limit > 0) resume(limit - 1) flatMap {
          case Left(value)  => List(Left(value.flatMap(g)))
          case Right(value) => g(value).resume(limit - 1)
        } else List(Left(result))
    )
    result
  }

  def map[A0 >: A, B](g: A0 => B): View[B] =
    flatMap((a: A0) => View.point(g(a)))

  final def forEach(stackLimit: Int)(run: A => Unit): Unit = {
    @tailrec def helper(elts: List[Either[View[A], A]]): Unit = elts match {
      case Nil           => ()
      case Right(a) :: t => run(a); helper(t)
      case Left(a) :: t  => helper(a.resume(stackLimit) ++ t)
    }
    helper(resume(stackLimit))
  }
}

object View {

  def apply[A](f: A*): View[A] = new View(_ => f.map(Right(_)).toList)

  def point[A](a: A): View[A] = View(a)

  val monad: Monad[View] = {
    new Monad[View] {
      def bind[A, B](fa: View[A])(f: A => View[B]): View[B] = fa.flatMap(f)
      def point[A](a: => A): View[A] = View(a)
    }
  }
}
