package nl.woupiestek.equalizer.parsing
import nl.woupiestek.equalizer.parsing.Parser._
import scala.collection.mutable
import scala.annotation.tailrec

sealed abstract class Parser[-I, +E, +A] {

  final def flatMap[I0 <: I, E0 >: E, A0 >: A, B](
      f: A0 => Parser[I0, E0, B]
  ): Parser[I0, E0, B] = this match {
    case Derive(d) => Derive((i: I0) => d(i).flatMap(f))
    case Empty     => Empty
    case Error(e)  => Error(e)
    case fm: FlatMap[I, E, b, A] =>
      FlatMap(fm.pa, fm.f(_: b).flatMap(f))
    case Point(a) => f(a)
    case s: Suspend[I, E, A] =>
      Suspend(() => s.value.flatMap(f))
    case _ => FlatMap(this, f)
  }

  final def map[A0 >: A, B](f: A0 => B): Parser[I, E, B] =
    flatMap((a0: A0) => Point(f(a0)))

  final def filter[A0 >: A](
      f: A0 => Boolean
  ): Parser[I, E, A0] =
    flatMap((a0: A0) => if (f(a0)) Point(a0) else Empty)

  final def plus[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser[I0, E0, A0]
  ): Parser[I0, E0, A0] = {
    this match {
      case Empty      => pb
      case Plus(l, r) => Plus(l, r.plus(pb))
      case s: Suspend[I, E, A] =>
        Suspend(() => s.value.plus(pb))
      case _ => Plus(this, pb)
    }
  }

  final def ++[I0 <: I, E0 >: E, A0 >: A](
      pb: => Parser[I0, E0, A0]
  ): Parser[I0, E0, A0] = plus(pb)

  private lazy val (ds, es, ps) = unfold(this)

  def derive(i: I): Parser[I, E, A] = {
    def helper(
        d: List[I => Parser[I, E, A]],
        out: => Parser[I, E, A]
    ): Parser[I, E, A] =
      d match {
        case Nil    => out
        case h :: t => helper(t, h(i).plus(out))
      }
    helper(ds, Empty)
  }

  def errors: List[E] = es.reverse
  def writes: List[A] = ps.reverse
}

object Parser {

  private final case class Derive[I, +E, +A](
      d: I => Parser[I, E, A]
  ) extends Parser[I, E, A]
  private final case object Empty
      extends Parser[Any, Nothing, Nothing]
  private final case class Error[+E](e: E)
      extends Parser[Any, E, Nothing]
  private final case class FlatMap[-I, +E, A, +B](
      pa: Parser[I, E, A],
      f: A => Parser[I, E, B]
  ) extends Parser[I, E, B]
  private final case class Plus[-I, +E, +A](
      l: Parser[I, E, A],
      r: Parser[I, E, A]
  ) extends Parser[I, E, A]
  private final case class Point[+A](a: A)
      extends Parser[Any, Nothing, A]
  private final case class Suspend[-I, +E, +A](
      private val fa: () => Parser[I, E, A]
  ) extends Parser[I, E, A] {
    lazy val value: Parser[I, E, A] = fa()
  }

  def empty[I, E, A]: Parser[I, E, A] = Empty
  def error[I, E, A](e: E): Parser[I, E, A] = Error(e)
  def point[I, E, A](a: A): Parser[I, E, A] = Point(a)
  def read[I, E]: Parser[I, E, I] = Derive(Point(_: I))
  def suspend[I, E, A](
      sa: => Parser[I, E, A]
  ): Parser[I, E, A] = Suspend(() => sa)

  def unfold[I, E, A](
      a: Parser[I, E, A]
  ): (List[I => Parser[I, E, A]], List[E], List[A]) = {
    val todo = new mutable.ArrayStack[Parser[I, E, A]]
    var points: List[A] = Nil
    var errors: List[E] = Nil
    var derive: List[I => Parser[I, E, A]] = Nil
    var limit: Int = (1 << 16)
    val done = new mutable.HashSet[Parser[I, E, A]]

    @tailrec def bind[B](
        pb: Parser[I, E, B],
        f: B => Parser[I, E, A]
    ): Unit =
      pb match {
        case Error(e) => errors ::= e
        case fm: FlatMap[I, E, c, B] =>
          bind(fm.pa, fm.f(_: c).flatMap(f))
        case Plus(l, r) =>
          if (r != Empty) todo.push(r.flatMap(f))
          bind(l, f)
        case Point(a) =>
          val fa = f(a)
          if (fa != Empty) todo.push(f(a))
        case s: Suspend[I, E, B] => bind(s.value, f)
        case _                   => if (pb != Empty) todo.push(pb.flatMap(f))
      }

    @tailrec def push(pa: Parser[I, E, A]): Unit =
      pa match {
        case Derive(d) => derive ::= d
        case Error(e)  => errors ::= e
        case fm: FlatMap[I, E, b, A] =>
          bind(fm.pa, fm.f)
        case Plus(l, r) =>
          if (r != Empty) todo.push(r)
          push(l)
        case Point(a)            => points ::= a
        case s: Suspend[I, E, A] => push(s.value)
        case _                   => if (pa != Empty) todo.push(pa)
      }

    push(a)
    while (todo.nonEmpty) {
      val next = todo.pop()
      if (todo.length > (1 << 16)) throw new SpaceOut
      if (!done.add(next)) throw new Cycle(next)
      if (limit > 0) limit -= 1 else throw new TimeOut
      push(next)
    }

    (derive, errors, points)
  }

  class Cycle(cycle: Parser[_, _, _])
      extends Exception(s"Cycle detected: $cycle")
  class SpaceOut
      extends Exception("Unfolding caused an explosion")
  class TimeOut extends Exception("Unfolding took too long")
}
