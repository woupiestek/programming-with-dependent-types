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
    case _          => if (pb == Empty) this else Plus(this, pb)
  }

  final def ++[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser2[I0, E0, A0]
  ): Parser2[I0, E0, A0] = plus(pb)

  private lazy val (ds, es, ps) = unfold(this)

  def derive(i: I): Parser2[I, E, A] =
    ds.foldLeft[Parser2[I, E, A]](Empty)(
      (x, y) => y(i).plus(x)
    )

  def errors: List[E] = es.reverse
  def writes: List[A] = ps.reverse
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

  private def unfold[I, E, A](
      a: Parser2[I, E, A]
  ): (List[(I => Parser2[I, E, A])], List[E], List[A]) = {
    val todo = new mutable.ArrayStack[Parser2[I, E, A]]
    var points: List[A] = Nil
    var errors: List[E] = Nil
    var derive: List[(I => Parser2[I, E, A])] = Nil
    var limit: Int = (1 << 16)

    def a2[B, C](
        pb: Parser2[I, E, B],
        pc: Parser2[I, E, C],
        f: (B, C) => A
    ): Unit =
      pb match {
        case a: Apply2[I, E, d, e, B] =>
          if (a.pa != Empty && a.pb != Empty)
            todo.push(
              apply2(
                a.pa,
                apply2(
                  a.pb,
                  pc,
                  (u: e, v: C) => (w: d) => f(a.f(w, u), v)
                ),
                (u: d, v: d => A) => v(u)
              )
            )
        case Empty    => ()
        case Error(e) => errors ::= e
        case Plus(l, r) =>
          if (r != Empty) todo.push(apply2(r, pc, f))
          if (l != Empty) todo.push(apply2(l, pc, f))
        case Point(b) =>
          pc match {
            case Point(c) => points ::= f(b, c)
            case _ =>
              if (pc != Empty)
                todo.push(
                  apply2(pc, pb, (u: C, v: B) => f(v, u))
                )
          }
        case ri: Read[I, E, B] =>
          derive ::= ((i: I) => apply2(ri.d(i), pc, f))
      }

    def push(pa: Parser2[I, E, A]): Unit =
      pa match {
        case a: Apply2[I, E, b, c, A] =>
          if (a.pb != Empty && a.pb != Empty)
            a2(a.pa, a.pb, a.f)
        case Empty    => ()
        case Error(e) => errors ::= e
        case Plus(l, r) =>
          if (r != Empty) todo.push(r)
          if (l != Empty) todo.push(l)
        case Point(a)          => points ::= a
        case ri: Read[I, E, A] => derive ::= ri.d
      }

    push(a)
    while (todo.nonEmpty) {
      if (todo.length > (1 << 16)) throw new SpaceOut
      if (limit > 0) limit -= 1 else throw new TimeOut
      push(todo.pop())
    }

    (derive, errors, points)
  }
  class SpaceOut
      extends Exception("Unfolding caused an explosion")
  class TimeOut extends Exception("Unfolding took too long")
}
