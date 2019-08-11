package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.collection.mutable

/* free applicative plus */
sealed abstract class Fap[+A] {
  def isEmpty: Boolean = this == Fap.Empty
}

object Fap {

  private final case object Empty extends Fap[Nothing]
  private final case class Point[A](a: A) extends Fap[A]
  private final case class Plus[A](
      left: Fap[A],
      right: Fap[A]
  ) extends Fap[A]
  private final case class Apply2[A, B, C](
      first: Fap[A],
      second: Fap[B],
      combine: (A, B) => C
  ) extends Fap[C]

  implicit val monadPlus: MonadPlus[Fap] =
    new MonadPlus[Fap] {
      override def ap[A, B](
          fa: => Fap[A]
      )(f: => Fap[A => B]): Fap[B] =
        fa match {
          case Empty => Empty
          case x: Apply2[c, d, A] =>
            val g = (u: d, v: A => B) =>
              (w: c) => v(x.combine(w, u))
            val h = (u: c, v: c => B) => v(u)
            Apply2(x.first, Apply2(x.second, f, g), h)
          case Point(a) => f.map(_(a))
          case _ =>
            val g = (u: A, v: A => B) => v(u)
            Apply2(fa, f, g)
        }
      def bind[A, B](fa: Fap[A])(f: A => Fap[B]): Fap[B] = {
        implicit val m: Monoid[Fap[B]] = monoid[B]
        fa.foldMap(f)
      }
      def empty[A]: Fap[A] = Empty
      def plus[A](a: Fap[A], b: => Fap[A]): Fap[A] =
        if (a.isEmpty) b
        else if (b.isEmpty) a
        else Plus(a, b)
      def point[A](a: => A): Fap[A] = Point(a)
    }

  implicit val foldable: Foldable[Fap] = new Foldable[Fap] {
    def foldMap[A, B](
        fa: Fap[A]
    )(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = f(a) |+| b
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fap[A], z: => B)(
        f: (A, => B) => B
    ): B = {
      val todo = new mutable.ArrayStack[Fap[A]]()
      todo.push(fa)
      var done: () => B = () => z

      def apply2[C, D](
          fc: Fap[C],
          fd: Fap[D],
          op: (C, D) => A
      ): Unit = fc match {
        case b: Apply2[e, f, c] =>
          val h1 = Apply2(
            b.second,
            fd,
            (v: f, w: D) => (u: e) => op(b.combine(u, v), w)
          )
          todo.push(b.first <*> h1)
        case Empty => ()
        case Plus(left, right) =>
          todo.push(Apply2(right, fd, op))
          todo.push(Apply2(left, fd, op))
        case Point(c0) =>
          fd match {
            case Point(d0) =>
              done = () => f(op(c0, d0), done())
            case _ =>
              todo.push(Apply2(fd, fc, op.flip))
          }
      }

      while (todo.nonEmpty) {
        todo.pop() match {
          case a: Apply2[c, d, A] =>
            apply2(a.first, a.second, a.combine)
          case Empty    => ()
          case Point(a) => done = () => f(a, done())
          case Plus(left, right) =>
            todo.push(right)
            todo.push(left)
        }
      }
      done()
    }
  }

}
