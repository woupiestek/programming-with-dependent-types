package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class Fmp[+O] {
  def isEmpty: Boolean = this == Fmp.Empty
}

object Fmp {

  private final case object Empty extends Fmp[Nothing]
  private final case class Plus[O](left: Fmp[O], right: Fmp[O]) extends Fmp[O]
  private final case class Point[+O](o: O) extends Fmp[O]
  private final case class FlatMap[O, P](dpo: Fmp[O], dpp: O => Fmp[P])
      extends Fmp[P]

  private val Pause: Fmp[Unit] = Point(())
  def suspend[I, O](dpo: => Fmp[O]): Fmp[O] =
    FlatMap(Pause, (_: Unit) => dpo)

  implicit val monadPlus: MonadPlus[Fmp] = new MonadPlus[Fmp] {
    def bind[A, B](fa: Fmp[A])(f: A => Fmp[B]): Fmp[B] =
      fa match {
        case Empty            => Empty
        case g: FlatMap[x, A] => FlatMap(g.dpo, g.dpp(_: x).flatMap(f))
        case Point(o)         => f(o)
        case _                => FlatMap(fa, f)
      }
    def empty[A]: Fmp[A] = Empty
    def plus[A](a: Fmp[A], b: => Fmp[A]): Fmp[A] =
      if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
    def point[A](a: => A): Fmp[A] = Point(a)
  }

  implicit val foldable: Foldable[Fmp] = new Foldable[Fmp] {
    def foldMap[A, B](fa: Fmp[A])(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = f(a) |+| b
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fmp[A], z: => B)(f: (A, => B) => B): B = {
      def helper1[C](pc: Fmp[C], g: C => Fmp[A], b: => B): B = pc match {
        case Empty             => b
        case fm: FlatMap[d, C] => helper1(fm.dpo, fm.dpp(_: d).flatMap(g), b)
        case Plus(left, right) => helper1(left, g, helper1(right, g, b))
        case Point(o)          => helper0(g(o), b)
      }

      def helper0(pa: Fmp[A], b: => B): B = pa match {
        case Empty             => b
        case FlatMap(dpo, dpp) => helper1(dpo, dpp, b)
        case Plus(left, right) => helper0(left, helper0(right, b))
        case Point(o)          => f(o, b)
      }
      helper0(fa, z)
    }
  }
}
