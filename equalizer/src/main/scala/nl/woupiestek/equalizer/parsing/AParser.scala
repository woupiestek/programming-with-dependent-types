package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class AParser[-I, +O] {
  def isEmpty: Boolean = this == AParser.Empty
}

object AParser {

  private final case object Empty extends AParser[Any, Nothing]
  //None to indicate a symbol should be left for another parser?
  private final case class ReadIf[I](d: I => Boolean) extends AParser[I, I]
  private final case class Plus[I, O](left: AParser[I, O], right: AParser[I, O])
      extends AParser[I, O]
  private final case class Write[O](o: O) extends AParser[Any, O]
  private final case class ZipWith[I, O, P, Q](
      first: AParser[I, O],
      second: AParser[I, P],
      combine: (O, P) => Q
  ) extends AParser[I, Q]

  val Void: AParser[Any, Unit] = Write(())
  def write[I, O](o: O): AParser[I, O] = Write(o)
  def empty[I, O]: AParser[I, O] = Empty
  def readIf[I](f: I => Boolean): AParser[I, I] = ReadIf(f)
  def suspend[I, O](o: => O): AParser[I, O] =
    ZipWith(Void, Void, (_: Unit, _: Unit) => o)

  private class Instances[I] {
    type F[+O] = AParser[I, O]
    val applicativePlus: ApplicativePlus[F] = new ApplicativePlus[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = fa match {
        case Empty    => empty
        case Write(a) => f.map((_: A => B)(a))
        case x: ZipWith[I, c, d, A] =>
          val g = (u: d, v: A => B) => (w: c) => v(x.combine(w, u))
          val h = (u: c, v: c => B) => v(u)
          ZipWith(x.first, ZipWith(x.second, f, g), h)
        case _ =>
          val g = (u: A, v: A => B) => v(u)
          ZipWith(fa, f, g)
      }
      override def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case Empty    => empty
        case Write(o) => point(f(o))
        case zw: ZipWith[I, c, d, A] =>
          val g = (u: c, v: d) => f(zw.combine(u, v))
          ZipWith(zw.first, zw.second, g)
        case _ => ZipWith(Void, fa, (_: Unit, a: A) => f(a))
      }
      def empty[A]: F[A] = Empty
      def plus[A](a: F[A], b: => F[A]): F[A] =
        if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
      def point[A](a: => A): F[A] = Write(a)
    }
  }

  implicit def applicativePlus[I]
      : ApplicativePlus[({ type F[+O] = AParser[I, O] })#F] =
    new Instances[I].applicativePlus
  implicit def monoid[I, O]: Monoid[AParser[I, O]] = applicativePlus.monoid[O]

  def fold[I, O, Z: Monoid](read: (I => Z) => Z, write: O => Z)(
      dp: AParser[I, O]
  ): Z = {
    def c[X, Y] = (x: X, y: X => Y) => y(x)

    def helper1[X](dpx: AParser[I, X], f: X => Z): Z = dpx match {
      case Empty             => Monoid[Z].zero
      case Plus(left, right) => helper1(left, f) |+| helper1(right, f)
      case ReadIf(g) =>
        read(
          (i: I) => if (g(i)) f(i.asInstanceOf[X]) else Monoid[Z].zero
        )
      case Write(o) => f(o)
      case zw: ZipWith[I, o, p, X] =>
        val g = (y: o, z: p) => f(zw.combine(y, z))
        helper2[o, p](zw.first, zw.second, g)
    }

    def helper2[X, Y](
        first: AParser[I, X],
        second: AParser[I, Y],
        combine: (X, Y) => Z
    ): Z =
      first match {
        case Empty => Monoid[Z].zero
        case Plus(left, right) =>
          helper2(left, second, combine) |+| helper2(right, second, combine)
        case ReadIf(d) =>
          read(
            (i: I) =>
              if (d(i)) helper1(second, combine(i.asInstanceOf[X], _))
              else Monoid[Z].zero
          )
        case Write(o) =>
          val g = (u: Y) => combine(o, u)
          helper1(second, g)
        case zw: ZipWith[I, o, p, X] =>
          val g = (x: p, y: Y) => (z: o) => combine(zw.combine(z, x), y)
          helper2(zw.first, ZipWith(zw.second, second, g), c[o, Z])
      }

    helper1(dp, write)
  }

  def derive[I, O](dp: AParser[I, O], i: I): AParser[I, O] =
    fold((_: I => AParser[I, O])(i), (_: O) => Empty)(dp)

  def matches[I, O](dp: AParser[I, O]): Stream[O] =
    fold((_: I => Stream[O]) => Stream.empty, Stream(_: O))(dp)

}
