package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

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
      @tailrec def helper(todo: List[Fmp[A]], done: List[A]): B = todo match {
        case Nil => done.foldLeft(z)((b, a) => f(a, b))
        case h :: t =>
          h match {
            case Empty => helper(t, done)
            case fm: FlatMap[b, A] =>
              fm.dpo match {
                case Empty => helper(t, done)
                case gm: FlatMap[c, d] =>
                  val h0 = gm.dpo.flatMap(gm.dpp(_: c).flatMap(fm.dpp))
                  helper(h0 :: t, done)
                case Plus(left, right) =>
                  val h0 = left.flatMap(fm.dpp)
                  val h1 = right.flatMap(fm.dpp)
                  helper(h0 :: h1 :: t, done)
                case Point(o) => helper(fm.dpp(o) :: t, done)
              }
            case Plus(left, right) => helper(left :: right :: t, done)
            case Point(o)          => helper(t, o :: done)
          }
      }

      helper(fa :: Nil, Nil)
    }
  }
}
