package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

sealed abstract class AParser[-I, +O] {
  def isEmpty: Boolean = this == AParser.Empty
  def isPause: Boolean = this == AParser.Pause
}

object AParser {

  private final case object Empty extends AParser[Any, Nothing]
  private final case object Pause extends AParser[Any, Unit]
  private final case class ReadIf[I](d: I => Boolean) extends AParser[I, I]
  private final case class Plus[I, O](left: AParser[I, O], right: AParser[I, O])
      extends AParser[I, O]
  private final case class Map2[I, O, P, Q](
      first: AParser[I, O],
      second: AParser[I, P],
      combine: (O, P) => Q
  ) extends AParser[I, Q]

  def readIf[I](f: I => Boolean): AParser[I, I] = ReadIf(f)

  private class Instances[I] {
    type F[+O] = AParser[I, O]
    val applicativePlus: ApplicativePlus[F] = new ApplicativePlus[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = fa match {
        case Empty => empty
        case x: Map2[I, c, d, A] =>
          val g = (u: d, v: A => B) => (w: c) => v(x.combine(w, u))
          val h = (u: c, v: c => B) => v(u)
          Map2(x.first, Map2(x.second, f, g), h)
        case Pause =>
          if (f.isPause) Pause.asInstanceOf[F[B]]
          else f.map(_(().asInstanceOf[A]))
        case _ =>
          val g = (u: A, v: A => B) => v(u)
          Map2(fa, f, g)
      }
      def empty[A]: F[A] = Empty
      def plus[A](a: F[A], b: => F[A]): F[A] =
        if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
      def point[A](a: => A): F[A] = Pause.map((_: Unit) => a)
    }
  }

  implicit def applicativePlus[I]
      : ApplicativePlus[({ type F[+O] = AParser[I, O] })#F] =
    new Instances[I].applicativePlus
  implicit def monoid[I, O]: Monoid[AParser[I, O]] = applicativePlus.monoid[O]

  def derive[I, O](i: I, p: AParser[I, O]): AParser[I, O] = {
    var in: AParser[I, O] = p
    @tailrec def map2[X, Y](
        x: AParser[I, X],
        y: AParser[I, Y],
        f: (X, Y) => O
    ): AParser[I, O] = x match {
      case m: Map2[I, c, d, X] =>
        val z =
          (m.second |@| y)((u: d, v: Y) => (w: c) => f(m.combine(w, u), v))
        map2(m.first, z, (u: c, v: c => O) => v(u))
      case Pause =>
        if (y.isPause) Empty else map2(y, x, (u: Y, v: X) => f(v, u))
      case Plus(left, right) =>
        in = (right |@| y)(f) <+> in
        map2(left, y, f)
      case ReadIf(g) if g(i) => y.map(f(i.asInstanceOf[X], _))
      case _                 => Empty
    }

    var out: AParser[I, O] = Empty
    while (!in.isEmpty) {
      val tmp = in
      in = Empty
      out = out <+> map2(tmp, Pause, (o: O, _: Unit) => o)
    }
    out
  }
}
