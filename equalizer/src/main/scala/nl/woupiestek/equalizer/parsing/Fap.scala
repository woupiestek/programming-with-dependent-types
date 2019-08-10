package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

/* free applicative plus */
sealed abstract class Fap[+A] {
  def isEmpty: Boolean = this == Fap.Empty
}

object Fap {

  private final case object Empty extends Fap[Nothing]
  private final case class Point[A](a: A) extends Fap[A]
  private final case class Plus[A](left: Fap[A], right: Fap[A]) extends Fap[A]
  private final case class Apply2[A, B, C](
      first: Fap[A],
      second: Fap[B],
      combine: (A, B) => C
  ) extends Fap[C]

  implicit val monadPlus: MonadPlus[Fap] =
    new MonadPlus[Fap] {
      override def ap[A, B](fa: => Fap[A])(f: => Fap[A => B]): Fap[B] =
        fa match {
          case Empty => Empty
          case x: Apply2[c, d, A] =>
            val g = (u: d, v: A => B) => (w: c) => v(x.combine(w, u))
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
        if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
      def point[A](a: => A): Fap[A] = Point(a)
    }

  implicit val foldable: Foldable[Fap] = new Foldable[Fap] {
    def foldMap[A, B](fa: Fap[A])(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = f(a) |+| b
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fap[A], z: => B)(f: (A, => B) => B): B = {

      @tailrec def helper1(todo: List[Fap[A]], done: List[A]): B = todo match {
        case Nil => done.foldLeft(z)((b, a) => f(a, b))
        case h :: t =>
          h match {
            case a: Apply2[c, d, A] =>
              a.first match {
                case b: Apply2[e, f, c] =>
                  val h1 = Apply2(
                    b.second,
                    a.second,
                    (v: f, w: d) => (u: e) => a.combine(b.combine(u, v), w)
                  )
                  val h0 = Apply2(b.first, h1, (u: e, v: e => A) => v(u))
                  helper1(h0 :: t, done)
                case Empty => helper1(t, done)
                case Plus(left, right) =>
                  val h0 = Apply2(left, a.second, a.combine)
                  val h1 = Apply2(right, a.second, a.combine)
                  helper1(h0 :: h1 :: t, done)
                case Point(c0) =>
                  a.second match {
                    case Point(d0) => helper1(t, a.combine(c0, d0) :: done)
                    case _ =>
                      val h0 = Apply2(a.second, a.first, a.combine.flip)
                      helper1(h0 :: t, done)
                  }
              }
            case Empty             => helper1(t, done)
            case Point(a)          => helper1(t, a :: done)
            case Plus(left, right) => helper1(left :: right :: t, done)
          }
      }

      helper1(fa :: Nil, Nil)
    }
  }

}
