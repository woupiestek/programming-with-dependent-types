package nl.woupiestek.equalizer.parsing
import nl.woupiestek.equalizer.parsing.Parser._
import scala.collection.mutable

sealed abstract class Parser[-I, +E, +A] {

  final def flatMap[I0 <: I, E0 >: E, A0 >: A, B](
      f: A0 => Parser[I0, E0, B]
  ): Parser[I0, E0, B] = this match {
    case Derive(d) => Derive(d(_: I0).flatMap(f))
    case Empty     => Empty
    case Error(e)  => Error(e)
    case fm: FlatMap[I, E, b, A] =>
      FlatMap(fm.pa, fm.f(_: b).flatMap(f))
    case Point(a) => f(a)
    case _        => FlatMap(this, f)
  }

  final def map[A0 >: A, B](f: A0 => B): Parser[I, E, B] =
    flatMap((a0: A0) => Point(f(a0)))

  final def filter[A0 >: A](
      f: A0 => Boolean
  ): Parser[I, E, A0] =
    flatMap((a0: A0) => if (f(a0)) Point(a0) else Empty)

  final def plus[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser[I0, E0, A0]
  ): Parser[I0, E0, A0] = this match {
    case Empty      => pb
    case Plus(l, r) => Plus(l, r.plus(pb))
    case _          => if (pb == Empty) this else Plus(this, pb)
  }

  final def ++[I0 <: I, E0 >: E, A0 >: A](
      pb: Parser[I0, E0, A0]
  ): Parser[I0, E0, A0] = plus(pb)

  private lazy val (ds, es, ps) = unfold(this)

  def derive(i: I): Parser[I, E, A] =
    ds.distinct.foldLeft[Parser[I, E, A]](Empty)(
      (x, y) => y(i).plus(x)
    )

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

  def empty[I, E, A]: Parser[I, E, A] = Empty
  def error[I, E, A](e: E): Parser[I, E, A] = Error(e)
  def point[I, E, A](a: A): Parser[I, E, A] = Point(a)
  def read[I, E]: Parser[I, E, I] = Derive(Point(_: I))

  private def unfold[I, E, A](
      a: Parser[I, E, A]
  ): (List[I => Parser[I, E, A]], List[E], List[A]) = {
    val todo = new mutable.ArrayStack[Parser[I, E, A]]
    var points: List[A] = Nil
    var errors: List[E] = Nil
    var derive: List[I => Parser[I, E, A]] = Nil
    var limit: Long = (1 << 20)

    def bind[B](
        pb: Parser[I, E, B],
        f: B => Parser[I, E, A]
    ): Unit =
      pb match {
        case Error(e) => errors ::= e
        case fm: FlatMap[I, E, c, B] =>
          if (fm.pa != Empty)
            todo.push(fm.pa.flatMap(fm.f(_: c).flatMap(f)))
        case Plus(l, r) =>
          if (r != Empty) todo.push(r.flatMap(f))
          if (l != Empty) todo.push(l.flatMap(f))
        case Point(a) =>
          val fa = f(a)
          if (fa != Empty) todo.push(fa)
        case _ =>
          if (pb != Empty) todo.push(pb.flatMap(f))
      }

    def push(pa: Parser[I, E, A]): Unit =
      pa match {
        case Derive(d) => derive ::= d
        case Empty     => ()
        case Error(e)  => errors ::= e
        case fm: FlatMap[I, E, b, A] =>
          if (fm.pa != Empty) bind(fm.pa, fm.f)
        case Plus(l, r) =>
          if (r != Empty) todo.push(r)
          if (l != Empty) todo.push(l)
        case Point(a) => points ::= a
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

  def parser3[I, E, A](
      parser: Parser[I, E, A]
  ): Parser3[I, Either[E, A]] = {
    Parser3 { f =>
      val store =
        new mutable.HashMap[(Int, Parser[I, E, Any]), Option[
          (Int, Either[E, Any])
        ]]

      def alternative[B](
          p: Parser[I, E, B],
          i: Int
      ): Option[(Int, Either[E, B])] =
        store.get((i, p)) match {
          case None =>
            val result = p match {
              case Derive(d) =>
                alternative(d(f(i)), i + 1)
              case Empty =>
                None
              case Error(e) => Some((i, Left(e)))
              case fm: FlatMap[I, E, c, B] =>
                alternative[c](fm.pa, i).flatMap {
                  case (j, Left(e)) => Some((j, Left(e)))
                  case (j, Right(a: c)) =>
                    alternative[B](fm.f(a), j)
                }
              case Plus(l, r) =>
                alternative(l, i) orElse alternative(r, i)
              case Point(a) => Some((i, Right(a)))
            }
            store.put((i, p), result)
            result
          case Some(result) => result.asInstanceOf[Option[(Int, Either[E, B])]]
        }

      alternative(parser, _)
    }
  }
}
