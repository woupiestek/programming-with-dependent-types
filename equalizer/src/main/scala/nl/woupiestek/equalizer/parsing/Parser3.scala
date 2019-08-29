package nl.woupiestek.equalizer.parsing
import nl.woupiestek.equalizer.parsing.Parser3._
import scala.collection.mutable
import scalaz.ApplicativePlus
import java.util.UUID

sealed abstract class Parser3[-I, +E, +A] {

  final def ap[I0 <: I, E0 >: E, A0 >: A, B](
      f: Parser3[I0, E0, A0 => B]
  ): Parser3[I0, E0, B] =
    apply2(this, f, (u: A, v: A => B) => v(u))

  final def map[A0 >: A, B](f: A0 => B): Parser3[I, E, B] =
    this match {
      case a: Apply2[I, E, b, c, A] =>
        apply2(a.pa, a.pb, (u: b, v: c) => f(a.f(u, v)))
      case Empty    => Empty
      case Error(e) => Error(e)
      case Point(a) => Point(f(a))
      case _        => ap(point(f))
    }

  final def plus[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser3[I0, E0, A0]
  ): Parser3[I0, E0, A0] = this match {
    case Empty      => pb
    case Plus(l, r) => Plus(l, r.plus(pb))
    case _ =>
      if (pb == Empty) this
      else Plus(this, pb)
  }

  final def ++[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser3[I0, E0, A0]
  ): Parser3[I0, E0, A0] = plus(pb)

  private lazy val (ds, es, ps) = unfold(this)

  def derive(i: I): Parser3[I, E, A] =
    ds.foldLeft[Parser3[I, E, A]](Empty)(
      (x, y) => if (y.l(i)) y.r.plus(x) else x
    )

  def errors: List[E] = es.reverse
  def writes: List[A] = ps.reverse

  final def flatMap[I0 <: I, E0 >: E, A0 >: A, B](
      f: A0 => Parser3[I0, E0, B]
  ): Parser3[I0, E0, B] = {
    val p: Parser3[I0, E0, B] =
      ps.foldLeft[Parser3[I0, E0, B]](Empty)(
        (x, y) => f(y).plus(x)
      )
    val e: Parser3[I0, E0, B] =
      es.foldLeft[Parser3[I0, E0, B]](p)(
        (x, y) => error(y).plus(x)
      )
    ds.foldLeft[Parser3[I0, E0, B]](e)(
      (x, y) => y.copy(r = y.r.flatMap(f)).plus(x)
    )
  }
}

object Parser3 {

  private final case class Apply2[-I, +E, A, B, +C](
      pa: Parser3[I, E, A],
      pb: Parser3[I, E, B],
      f: (A, B) => C
  ) extends Parser3[I, E, C]
  private final case object Empty
      extends Parser3[Any, Nothing, Nothing]
  private final case class Error[+E](e: E)
      extends Parser3[Any, E, Nothing]
  private final case class Plus[-I, +E, +A](
      l: Parser3[I, E, A],
      r: Parser3[I, E, A]
  ) extends Parser3[I, E, A]
  private final case class Point[+A](a: A)
      extends Parser3[Any, Nothing, A]
  private final case class ReadIf[-I, +E, +A](
      l: I => Boolean,
      r: Parser3[I, E, A]
  ) extends Parser3[I, E, A]

  private final case class Loop[I, E, A](
      label: UUID,
      parser: () => Parser3[I, E, A]
  ) extends Parser3[I, E, A]

  private def apply2[I, E, A, B, C](
      pa: Parser3[I, E, A],
      pb: Parser3[I, E, B],
      f: (A, B) => C
  ): Parser3[I, E, C] = pa match {
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
  def empty[I, E, A]: Parser3[I, E, A] = Empty
  def error[I, E, A](e: E): Parser3[I, E, A] = Error(e)
  def loop[I, E, A](
      f: Parser3[I, E, A] => Parser3[I, E, A]
  ): Parser3[I, E, A] = {
    val id = UUID.randomUUID()
    lazy val l: Parser3[I, E, A] = Loop(id, () => f(l))
    l
  }
  def point[I, E, A](a: A): Parser3[I, E, A] = Point(a)
  def readIf[I, E, A](
      f: I => Boolean,
      p: Parser3[I, E, A]
  ): Parser3[I, E, A] = ReadIf(f, p)

  implicit def applicativePlus[I, E]: ApplicativePlus[
    ({ type P[A] = Parser3[I, E, A] })#P
  ] = {
    type P[A] = Parser3[I, E, A]
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
      a: Parser3[I, E, A]
  ): (List[ReadIf[I, E, A]], List[E], List[A]) = {
    val todo = new mutable.ArrayStack[Parser3[I, E, A]]
    var points: List[A] = Nil
    var errors: List[E] = Nil
    var derive: List[ReadIf[I, E, A]] = Nil
    var limit: Long = (1 << 20)

    def a2[B, C](
        pb: Parser3[I, E, B],
        pc: Parser3[I, E, C],
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
        case l: Loop[I, E, B] =>
          val p = l.parser()
          if (p != empty)
            todo.push(apply2(p, pc, f))
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
        case ReadIf(g, pd) =>
          derive ::= ReadIf(g, apply2(pd, pc, f))
      }

    def push(pa: Parser3[I, E, A]): Unit =
      pa match {
        case a: Apply2[I, E, b, c, A] =>
          if (a.pb != Empty && a.pb != Empty)
            a2(a.pa, a.pb, a.f)
        case Empty    => ()
        case Error(e) => errors ::= e
        case l: Loop[I, E, A] =>
          val p = l.parser()
          if (p != Empty) todo.push(p)
        case Plus(l, r) =>
          if (r != Empty) todo.push(r)
          if (l != Empty) todo.push(l)
        case Point(a)            => points ::= a
        case ri: ReadIf[I, E, A] => derive ::= ri
      }

    push(a)
    while (todo.nonEmpty) {
      if (todo.length > (1 << 16)) throw new SpaceOut
      if (limit > 0) limit -= 1
      else {
        println("Draining todo's")
        todo.drain(println)
        throw new TimeOut
      }
      push(todo.pop())
    }

    (derive, errors, points)
  }
  class SpaceOut
      extends Exception("Unfolding caused an explosion")
  class TimeOut extends Exception("Unfolding took too long")
}
