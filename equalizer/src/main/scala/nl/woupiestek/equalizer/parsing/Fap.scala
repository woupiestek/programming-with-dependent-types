package nl.woupiestek.equalizer.parsing

import scalaz._
import scala.collection.mutable
import scala.annotation.tailrec

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
  private final case class Suspend[+O](
      private val fo: () => Fap[O]
  ) extends Fap[O] {
    lazy val value: Fap[O] = fo()
  }

  private def suspend[A](fa: => Fap[A]): Fap[A] =
    Suspend(() => fa)

  implicit val monadPlus: MonadPlus[Fap] =
    new MonadPlus[Fap] {
      override def ap[A, B](
          fa: => Fap[A]
      )(f: => Fap[A => B]): Fap[B] =
        Apply2(
          suspend(fa),
          suspend(f),
          (u: A, v: A => B) => v(u)
        )
      def bind[A, B](fa: Fap[A])(f: A => Fap[B]): Fap[B] = {
        implicit val m: Monoid[Fap[B]] = monoid[B]
        foldable.foldMap(fa)(f)
      }
      def empty[A]: Fap[A] = Empty
      def plus[A](a: Fap[A], b: => Fap[A]): Fap[A] =
        Plus(a, suspend(b))
      def point[A](a: => A): Fap[A] = Point(a)
    }

  implicit val foldable: Foldable[Fap] = new Foldable[Fap] {
    def foldMap[A, B](
        fa: Fap[A]
    )(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = F.append(f(a), b)
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fap[A], z: => B)(
        f: (A, => B) => B
    ): B = {
      val todo = new mutable.ArrayStack[Fap[A]]()
      var result: () => B = () => z
      val done = new mutable.HashSet[Fap[A]]

      @tailrec def apply2[C, D](
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
          apply2(b.first, h1, (u: e, v: e => A) => v(u))
        case Empty => ()
        case Plus(left, right) =>
          todo.push(Apply2(right, fd, op))
          apply2(left, fd, op)
        case Point(c0) =>
          fd match {
            case Point(d0) =>
              result = () => f(op(c0, d0), result())
            case _ =>
              apply2(fd, fc, (d: D, c: C) => op(c, d))
          }
        case s: Suspend[C] => apply2(s.value, fd, op)
      }

      @tailrec def run(next: Fap[A]): Unit = {
        if (!done.add(next)) throw new Cycle()
        next match {
          case a: Apply2[c, d, A] =>
            apply2(a.first, a.second, a.combine)
          case Empty    => ()
          case Point(a) => result = () => f(a, result())
          case Plus(left, right) =>
            todo.push(right)
            run(left)
          case s: Suspend[A] => run(s.value)
        }
      }

      run(fa)
      while (todo.nonEmpty) {
        run(todo.pop())
      }
      result()
    }
  }

  class Cycle extends Exception("Cycle detected")
}
