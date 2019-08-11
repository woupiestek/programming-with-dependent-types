package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.collection.mutable
import scala.annotation.tailrec

sealed abstract class Fmp[+O]

object Fmp {

  private final case object Empty extends Fmp[Nothing]
  private final case class Plus[O](
      left: Fmp[O],
      right: Fmp[O]
  ) extends Fmp[O]
  private final case class Point[+O](o: O) extends Fmp[O]
  private final case class FlatMap[O, P](
      dpo: Fmp[O],
      dpp: O => Fmp[P]
  ) extends Fmp[P]

  implicit val monadPlus: MonadPlus[Fmp] =
    new MonadPlus[Fmp] {
      def bind[A, B](fa: Fmp[A])(f: A => Fmp[B]): Fmp[B] =
        FlatMap(fa, f)
      def empty[A]: Fmp[A] = Empty
      def plus[A](a: Fmp[A], b: => Fmp[A]): Fmp[A] =
        Plus(a, b)
      def point[A](a: => A): Fmp[A] = Point(a)
    }

  implicit val foldable: Foldable[Fmp] = new Foldable[Fmp] {
    def foldMap[A, B](
        fa: Fmp[A]
    )(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = f(a) |+| b
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fmp[A], z: => B)(
        f: (A, => B) => B
    ): B = {
      val todo = new mutable.ArrayStack[Fmp[A]]
      todo.push(fa)
      var done: () => B = () => z

      @tailrec def bind[C](
          fc: Fmp[C],
          g: C => Fmp[A]
      ): Unit =
        fc match {
          case Empty => ()
          case gm: FlatMap[c, d] =>
            bind(gm.dpo, gm.dpp(_: c).flatMap(g))
          case Plus(left, right) =>
            todo.push(right.flatMap(g))
            bind(left, g)
          case Point(o) =>
            todo.push(g(o))
        }

      while (todo.nonEmpty) {
        todo.pop() match {
          case Empty             => ()
          case fm: FlatMap[b, A] => bind(fm.dpo, fm.dpp)
          case Plus(left, right) =>
            todo.push(right)
            todo.push(left)
          case Point(o) => done = () => f(o, done())
        }
      }
      done()
    }
  }
}
