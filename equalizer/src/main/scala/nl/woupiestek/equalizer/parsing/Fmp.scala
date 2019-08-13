package nl.woupiestek.equalizer.parsing

import scalaz._
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
  private final case class Suspend[+O](
      private val fo: () => Fmp[O]
  ) extends Fmp[O] {
    lazy val value: Fmp[O] = fo()
  }

  private def suspend[A](fa: => Fmp[A]): Fmp[A] =
    Suspend(() => fa)

  implicit val monadPlus: MonadPlus[Fmp] =
    new MonadPlus[Fmp] {
      def bind[A, B](fa: Fmp[A])(f: A => Fmp[B]): Fmp[B] =
        fa match {
          case Empty => Empty
          case fm: FlatMap[b, A] =>
            FlatMap(fm.dpo, (u: b) => bind(fm.dpp(u))(f))
          case Point(a)      => f(a)
          case s: Suspend[A] => suspend(bind(s.value)(f))
          case _             => FlatMap(fa, f)
        }
      def empty[A]: Fmp[A] = Empty
      def plus[A](a: Fmp[A], b: => Fmp[A]): Fmp[A] =
        a match {
          case Empty => suspend(b)
          case Plus(left, right) =>
            Plus(left, Plus(right, suspend(b)))
          case _ => Plus(a, suspend(b))
        }
      def point[A](a: => A): Fmp[A] = suspend(Point(a))
    }

  implicit val foldable: Foldable[Fmp] = new Foldable[Fmp] {
    def foldMap[A, B](
        fa: Fmp[A]
    )(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = F.append(f(a), b)
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fmp[A], z: => B)(
        f: (A, => B) => B
    ): B = {
      val todo = new mutable.ArrayStack[Fmp[A]]
      var result: () => B = () => z
      val done = new mutable.HashSet[Fmp[A]]

      @tailrec def push(fa: Fmp[A]): Unit = fa match {
        case Plus(left, right) =>
          if (right != Empty) todo.push(right)
          push(left)
        case Point(o)      => result = () => f(o, result())
        case s: Suspend[A] => push(s.value)
        case _             => if (fa != Empty) todo.push(fa)
      }

      @tailrec def bind[C](
          fc: Fmp[C],
          g: C => Fmp[A]
      ): Unit =
        fc match {
          case Empty => ()
          case gm: FlatMap[c, d] =>
            bind(
              gm.dpo,
              (u: c) => monadPlus.bind(gm.dpp(u))(g)
            )
          case Plus(left, right) =>
            push(monadPlus.bind(right)(g))
            bind(left, g)
          case Point(o)      => push(g(o))
          case s: Suspend[C] => bind(s.value, g)
        }

      //val console = System.console()
      //var counter = 1 << 16

      def run(next: Fmp[A]): Unit = {
        /* if (counter > 0) {
          counter -= 1
        } else {
          console.printf(s"\rfolding: ${todo.length} $next")
          counter = 1 << 16
        } */
        if (!done.add(next)) throw new Cycle(next)
        next match {
          case fm: FlatMap[b, A] => bind(fm.dpo, fm.dpp)
          case _                 => push(next)
        }
      }

      run(fa)
      while (todo.nonEmpty) {
        run(todo.pop())
      }
      result()
    }
  }

  case class Cycle(cycle: Fmp[_])
      extends Exception(s"Cycle detected: $cycle")
}
