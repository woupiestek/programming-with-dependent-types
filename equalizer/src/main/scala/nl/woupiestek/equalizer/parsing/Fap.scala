package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

/* free applicative plus */
sealed abstract class Fap[+A] {
  def isEmpty: Boolean = this == Fap.Empty
  def isPause: Boolean = this == Fap.Pause
}

object Fap {

  private final case object Empty extends Fap[Nothing]
  private final case object Pause extends Fap[Unit]
  private final case class Plus[A](left: Fap[A], right: Fap[A]) extends Fap[A]
  private final case class Apply2[A, B, C](
      first: Fap[A],
      second: Fap[B],
      combine: (A, B) => C
  ) extends Fap[C]

  implicit val applicativePlus: ApplicativePlus[Fap] =
    new ApplicativePlus[Fap] {
      def ap[A, B](fa: => Fap[A])(f: => Fap[A => B]): Fap[B] = fa match {
        case Empty => Empty
        case x: Apply2[c, d, A] =>
          val g = (u: d, v: A => B) => (w: c) => v(x.combine(w, u))
          val h = (u: c, v: c => B) => v(u)
          Apply2(x.first, Apply2(x.second, f, g), h)
        case Pause => f.map(_(().asInstanceOf[A]))
        case _ =>
          val g = (u: A, v: A => B) => v(u)
          Apply2(fa, f, g)
      }
      def empty[A]: Fap[A] = Empty
      def plus[A](a: Fap[A], b: => Fap[A]): Fap[A] =
        if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
      def point[A](a: => A): Fap[A] = Pause.map((_: Unit) => a)
    }

  implicit val foldable: Foldable[Fap] = new Foldable[Fap] {
    def foldMap[A, B](fa: Fap[A])(f: A => B)(implicit F: Monoid[B]): B = {
      def g(a: A, b: => B): B = f(a) |+| b
      foldRight(fa, Monoid[B].zero)(g)
    }

    def foldRight[A, B](fa: Fap[A], z: => B)(f: (A, => B) => B): B = {
      def helper[C, D](fc: Fap[C], fd: Fap[D], op: (C, D) => A, b: => B): B =
        fc match {
          case Empty => b
          case m: Apply2[e, f, C] =>
            val g = Apply[Fap].apply2(m.second, fd)(
              (u: f, v: D) => (w: e) => op(m.combine(w, u), v)
            )
            helper(m.first, g, (u: e, v: e => A) => v(u), b)
          case Pause =>
            if (fd.isPause) f(op((), ().asInstanceOf[D]), b)
            else helper(fd, fc, (u: D, v: C) => op(v, u), b)
          case Plus(left, right) =>
            helper(left, fd, op, helper(right, fd, op, b))
        }

      def helper2(ff: Fap[A], b: => B): B = ff match {
        case Empty              => b
        case m: Apply2[c, d, A] => helper(m.first, m.second, m.combine, b)
        case Pause              => f((), z)
        case Plus(left, right) =>
          helper2(left, helper2(right, b))
      }

      helper2(fa, z)
    }
  }

}
