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

  implicit val monadPlus: MonadPlus[Fap] =
    new MonadPlus[Fap] {
      def bind[A, B](fa: Fap[A])(f: A => Fap[B]): Fap[B] = {
        implicit val m: Monoid[Fap[B]] = monoid[B]
        fa match {
          case Empty    => Empty
          case Point(a) => f(a)
          case _        => foldable.foldMap(fa)(f)
        }
      }
      def empty[A]: Fap[A] = Empty
      def plus[A](a: Fap[A], b: => Fap[A]): Fap[A] =
        a match {
          case Empty => b
          case Plus(left, right) =>
            Plus(left, Plus(right, b))
          case _ => Plus(a, b)
        }
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

      @tailrec def push(fa: Fap[A]): Unit = fa match {
        case Plus(left, right) =>
          if (right != Empty) todo.push(right)
          push(left)
        case Point(o) => result = () => f(o, result())
        case _        => if (fa != Empty) todo.push(fa)
      }

      def run(next: Fap[A]): Unit = {
        Tracer.log(s"\rfolding: ${todo.length} $next")
        if (!done.add(next)) throw new Cycle(next)
        next match {
          case _ => push(next)
        }
      }

      run(fa)
      while (todo.nonEmpty) {
        run(todo.pop())
      }
      result()
    }
  }

  case class Cycle(cycle: Fap[_])
      extends Exception(s"Cycle detected: $cycle")
}
