package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class AParser[-I, +O] {
  def isEmpty: Boolean = this == AParser.Empty
  def isPoint: Boolean = this.isInstanceOf[AParser.Point[O]]
}

object AParser {

  private final case object Empty extends AParser[Any, Nothing]
  private final case class Point[+O](o: O) extends AParser[Any, O]
  private final case class Ask[-I, O](d: I => AParser[I, O])
      extends AParser[I, O]
  private final case class Plus[I, O](left: AParser[I, O], right: AParser[I, O])
      extends AParser[I, O]
  private final case class Apply2[I, O, P, Q](
      first: AParser[I, O],
      second: AParser[I, P],
      combine: (O, P) => Q
  ) extends AParser[I, Q]

  private class Instances[I] {
    type F[+O] = AParser[I, O]
    val applicativePlus: ApplicativePlus[F] = new ApplicativePlus[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = fa match {
        case Ask(d) => Ask((i: I) => ap(d(i))(f))
        case Empty  => Empty
        case x: Apply2[I, c, d, A] =>
          def g(u: d, v: A => B) = (w: c) => v(x.combine(w, u))
          def h(u: c, v: c => B) = v(u)
          Apply2(x.first, Apply2(x.second, f, g), h)
        case Point(a) =>
          f match {
            case Point(g) => Point(g(a))
            case _        => f.map(_(a))
          }
        case _ =>
          def g(u: A, v: A => B) = v(u)
          Apply2(fa, f, g)
      }
      def empty[A]: F[A] = Empty
      def plus[A](a: F[A], b: => F[A]): F[A] =
        if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
      def point[A](a: => A): F[A] = Point(a)
    }
  }

  implicit def applicativePlus[I]
      : ApplicativePlus[({ type F[+O] = AParser[I, O] })#F] =
    new Instances[I].applicativePlus

  implicit def monoid[I, O]: Monoid[AParser[I, O]] = applicativePlus.monoid[O]

  def foldMap[I, A, B](i: I, fa: AParser[I, A])(
      f: A => B
  )(implicit F: Monoid[B]): B = {
    def g(a: A, b: => B) = f(a) |+| b
    foldRight(i, fa, Monoid[B].zero)(g)
  }

  def foldRight[I, A, B](i: I, fa: AParser[I, A], z: => B)(
      f: (A, => B) => B
  ): B = {
    def helper[C, D](
        fc: AParser[I, C],
        fd: AParser[I, D],
        op: (C, D) => A,
        b: => B
    ): B =
      fc match {
        case a: Apply2[I, e, f, C] =>
          def g(u: f, v: D) = (w: e) => op(a.combine(w, u), v)
          helper(
            a.first,
            Apply2(a.second, fd, g),
            (u: e, v: e => A) => v(u),
            b
          )
        case Ask(d) => helper(d(i), fd, op, b)
        case Empty  => b
        case Point(c) =>
          fd match {
            case Point(d) => f(op(c, d), b)
            case _        => helper(fd, fc, (u: D, v: C) => op(v, u), b)
          }
        case Plus(left, right) =>
          helper(left, fd, op, helper(right, fd, op, b))
      }

    def helper2(a: AParser[I, A], b: => B): B = a match {
      case a: Apply2[I, c, d, A] => helper(a.first, a.second, a.combine, b)
      case Ask(d)                => helper2(d(i), b)
      case Empty                 => b
      case Point(a0)             => f(a0, b)
      case Plus(left, right)     => helper2(left, helper2(right, b))
    }
    helper2(fa, z)
  }

}
