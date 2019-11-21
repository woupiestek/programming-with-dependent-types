package nl.woupiestek.equalizer.parsing
import nl.woupiestek.equalizer.parsing.Parser2._
import scala.collection.mutable
import scalaz.ApplicativePlus

sealed abstract class Parser2[-I, +E, +A] {

  final def ap[I0 <: I, E0 >: E, A0 >: A, B](
      f: Parser2[I0, E0, A0 => B]
  ): Parser2[I0, E0, B] =
    apply2(this, f, (u: A, v: A => B) => v(u))

  final def map[A0 >: A, B](f: A0 => B): Parser2[I, E, B] =
    this match {
      case a: Apply2[I, E, b, c, A] =>
        apply2(a.pa, a.pb, (u: b, v: c) => f(a.f(u, v)))
      case Empty    => Empty
      case Error(e) => Error(e)
      case Point(a) => Point(f(a))
      case _        => ap(point(f))
    }

  final def plus[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser2[I0, E0, A0]
  ): Parser2[I0, E0, A0] = this match {
    case Empty      => pb
    case Plus(l, r) => Plus(l, r.plus(pb))
    case _ =>
      if (pb == Empty) this
      else Plus(this, pb)
  }

  final def ++[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser2[I0, E0, A0]
  ): Parser2[I0, E0, A0] = plus(pb)

}

object Parser2 {

  private final case object Empty
      extends Parser2[Any, Nothing, Nothing]
  private final case class Error[+E](e: E)
      extends Parser2[Any, E, Nothing]
  private final case class Apply2[-I, +E, A, B, +C](
      pa: Parser2[I, E, A],
      pb: Parser2[I, E, B],
      f: (A, B) => C
  ) extends Parser2[I, E, C]
  private final case class Plus[-I, +E, +A](
      l: Parser2[I, E, A],
      r: Parser2[I, E, A]
  ) extends Parser2[I, E, A]
  private final case class Point[+A](a: A)
      extends Parser2[Any, Nothing, A]
  private final case class Read[-I, +E, +A](
      d: I => Parser2[I, E, A]
  ) extends Parser2[I, E, A]

  private def apply2[I, E, A, B, C](
      pa: Parser2[I, E, A],
      pb: Parser2[I, E, B],
      f: (A, B) => C
  ): Parser2[I, E, C] = pa match {
    case a: Apply2[I, E, d, e, A] =>
      Apply2(
        a.pa,
        Apply2(
          a.pb,
          pb,
          (u: e, v: B) => (w: d) => f(a.f(w, u), v)
        ),
        (u: d, v: d => C) => v(u)
      )
    case Empty => Empty
    case Point(a) =>
      pb match {
        case Point(b) => Point(f(a, b))
        case _        => Apply2(pb, pa, (u: B, v: A) => f(v, u))
      }
    case _ => Apply2(pa, pb, f)
  }
  def empty[I, E, A]: Parser2[I, E, A] = Empty
  def error[I, E, A](e: E): Parser2[I, E, A] = Error(e)
  def point[I, E, A](a: A): Parser2[I, E, A] = Point(a)
  def read[I, E, A](
      f: I => Parser2[I, E, A]
  ): Parser2[I, E, A] = Read(f)

  implicit def applicativePlus[I, E]: ApplicativePlus[
    ({ type P[A] = Parser2[I, E, A] })#P
  ] = {
    type P[A] = Parser2[I, E, A]
    new ApplicativePlus[P] {
      def ap[A, B](fa: => P[A])(f: => P[A => B]): P[B] =
        Apply2(
          fa,
          f,
          (u: A, v: A => B) => v(u)
        )
      def empty[A]: P[A] = Empty
      def plus[A](a: P[A], b: => P[A]): P[A] =
        a.plus(b)
      def point[A](a: => A): P[A] =
        Point(a)
    }
  }

  def parser3[I, E, A](
      parser: Parser2[I, E, A]
  ): Parser3[I, Either[E, A]] = {
    Parser3 { f =>
      val store =
        new mutable.HashMap[(Int, Parser2[I, E, Any]), Option[
          (Int, Either[E, Any])
        ]]

      def alternative[B](
          p: Parser2[I, E, B],
          i: Int
      ): Option[(Int, Either[E, B])] =
        store
          .getOrElseUpdate(
            (i, p),
            p match {
              case Apply2(pa, pb, g) =>
                alternative(pa, i).flatMap {
                  case (j, Left(e)) => Some((j, Left(e)))
                  case (j, Right(a)) =>
                    alternative(pb, j).map {
                      case (k, Left(e))  => (k, Left(e))
                      case (k, Right(b)) => (k, Right(g(a, b)))
                    }
                }
              case Empty    => None
              case Error(e) => Some((i, Left(e)))
              case Plus(l, r) =>
                alternative(l, i) orElse alternative(r, i)
              case Point(a) => Some((i, Right(a)))
              case Read(d)  => alternative(d(f(i)), i + 1)
            }
          )
          .asInstanceOf[Option[(Int, Either[E, B])]]

      alternative(parser, _)

    }
  }

}
