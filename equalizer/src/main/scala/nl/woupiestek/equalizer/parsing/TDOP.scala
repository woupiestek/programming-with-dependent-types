package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class TDOP[-I, +O]

object TDOP {

  private final case class Error(message: String) extends TDOP[Any, Nothing]
  //None to indicate a symbol should be left for another parser?
  private final case class Read[I, O](d: I => TDOP[I, O]) extends TDOP[I, O]
  private final case class Point[+O](o: O) extends TDOP[Any, O]
  private final case class Map[I, O, P](
      first: TDOP[I, O],
      combine: O => P
  ) extends TDOP[I, P]
  private final case class Map2[I, O, P, Q](
      first: TDOP[I, O],
      second: TDOP[I, P],
      combine: (O, P) => Q
  ) extends TDOP[I, Q]

  val Void: TDOP[Any, Unit] = Point(())
  def write[I, O](o: O): TDOP[I, O] = Point(o)
  def error[I, O](message: String): TDOP[I, O] = Error(message)
  def read[I, O](f: I => TDOP[I, O]): TDOP[I, O] = Read(f)
  def suspend[I, O](o: => O): TDOP[I, O] =
    Map2(Void, Void, (_: Unit, _: Unit) => o)
  def zipWith[I, A, B, C](
      fa: TDOP[I, A],
      f: TDOP[I, B],
      g: (A, B) => C
  ): TDOP[I, C] = fa match {
    case m: Map[I, o, A] =>
      val h = (x: o, y: B) => g(m.combine(x), y)
      Map2(m.first, f, h)
    case Read(d)  => Read((i: I) => zipWith(d(i), f, g))
    case Point(o) => f.map(g(o, _))
    case zw: Map2[I, o, p, A] =>
      val h = (x: p, y: B) => (z: o) => g(zw.combine(z, x), y)
      Map2(zw.first, zipWith(zw.second, f, h), c[o, C])
    case _ => Map2(fa, f, g)
  }

  private class Instances[I] {
    type F[+O] = TDOP[I, O]
    val applicative: Applicative[F] = new Applicative[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = fa match {
        case m: Map[I, c, A] =>
          val g = (u: c, v: A => B) => v(m.combine(u))
          Map2(m.first, f, g)
        case Read(d)  => read((i: I) => ap(d(i))(f))
        case Point(a) => f.map((_: A => B)(a))
        case x: Map2[I, c, d, A] =>
          val g = (u: d, v: A => B) => (w: c) => v(x.combine(w, u))
          val h = (u: c, v: c => B) => v(u)
          Map2(x.first, zipWith(x.second, f, g), h)
        case _ =>
          val g = (u: A, v: A => B) => v(u)
          Map2(fa, f, g)
      }
      override def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case m: Map[I, c, A] => Map(m.first, m.combine andThen f)
        case Read(d)         => read((i: I) => map(d(i))(f))
        case Point(o)        => point(f(o))
        case zw: Map2[I, c, d, A] =>
          val g = (u: c, v: d) => f(zw.combine(u, v))
          Map2(zw.first, zw.second, g)
        case _ => Map(fa, f)
      }
      def point[A](a: => A): F[A] = Point(a)

    }
  }

  implicit def applicative[I]: Applicative[({ type F[+O] = TDOP[I, O] })#F] =
    new Instances[I].applicative

  def derive[I, O](dp: TDOP[I, O])(i: I): Either[String, TDOP[I, O]] = {

    def helper1[X](dpx: TDOP[I, X], f: X => O): Either[String, TDOP[I, O]] =
      dpx match {
        case m: Map[I, o, X] => helper1(m.first, m.combine andThen f)
        case Error(msg)      => Left(msg)
        case Read(g)         => Right(g(i).map(f))
        case Point(o)        => Right(write(f(o)))
        case zw: Map2[I, o, p, X] =>
          val g = (y: o, z: p) => f(zw.combine(y, z))
          helper2[o, p](zw.first, zw.second, g)
      }

    def helper2[X, Y](
        first: TDOP[I, X],
        second: TDOP[I, Y],
        combine: (X, Y) => O
    ): Either[String, TDOP[I, O]] =
      first match {
        case m: Map[I, o, X] =>
          val g = (x: o, y: Y) => combine(m.combine(x), y)
          helper2(m.first, second, g)
        case Error(msg) => Left(msg)
        case Read(d)    => Right(zipWith(d(i), second, combine))
        case Point(o) =>
          val g = (u: Y) => combine(o, u)
          helper1(second, g)
        case zw: Map2[I, o, p, X] =>
          val g = (x: p, y: Y) => (z: o) => combine(zw.combine(z, x), y)
          helper2(zw.first, zipWith(zw.second, second, g), c[o, O])
      }

    helper1(dp, (o: O) => o)
  }

  def fetch[I, O](
      dp: TDOP[I, O]
  )(underrun: String): Either[String, O] = {

    def helper1[X](dpx: TDOP[I, X], f: X => O): Either[String, O] = dpx match {
      case m: Map[I, o, X] => helper1(m.first, m.combine andThen f)
      case Error(msg)      => Left(msg)
      case Read(_)         => Left(underrun)
      case Point(o)        => Right(f(o))
      case zw: Map2[I, o, p, X] =>
        val g = (y: o, z: p) => f(zw.combine(y, z))
        helper2[o, p](zw.first, zw.second, g)
    }

    def helper2[X, Y](
        first: TDOP[I, X],
        second: TDOP[I, Y],
        combine: (X, Y) => O
    ): Either[String, O] =
      first match {
        case Error(msg) => Left(msg)
        case m: Map[I, o, X] =>
          val g = (x: o, y: Y) => combine(m.combine(x), y)
          helper2(m.first, second, g)
        case Read(_) => Left(underrun)
        case Point(o) =>
          val g = (u: Y) => combine(o, u)
          helper1(second, g)
        case zw: Map2[I, o, p, X] =>
          val g = (x: p, y: Y) => (z: o) => combine(zw.combine(z, x), y)
          helper2(zw.first, zipWith(zw.second, second, g), c[o, O])
      }

    helper1(dp, (o: O) => o)
  }
  private def c[X, Y] = (x: X, y: X => Y) => y(x)
}
