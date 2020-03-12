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

  def parse[I, E, A](
      parser: Parser[I, E, A],
      input: Int => I,
      index: Int
  ): Option[(Int, Either[E, A])] = {
    var stack: List[(Parser[I, E, A], Int)] = Nil

    def _parse(
        parser: Parser[I, E, A],
        index: Int
    ): Option[(Int, Either[E, A])] =
      parser match {
        case Derive(d) => _parse(d(input(index)), index + 1)
        case Empty =>
          stack match {
            case Nil => None
            case (head0, head1) :: next =>
              stack = next
              _parse(head0, head1)
          }
        case Error(e) => Some((index, Left(e)))
        case FlatMap(pa, f) =>
          pa match {
            case Derive(d) =>
              _parse(d(input(index)).flatMap(f), index + 1)
            case Empty    => None
            case Error(e) => Some((index, Left(e)))
            case fm: FlatMap[I, E, a, b] =>
              _parse(
                fm.pa.flatMap(fm.f(_: a).flatMap(f)),
                index
              )
            case Plus(l, r) =>
              _parse(l.flatMap(f) ++ r.flatMap(f), index)
            case Point(a) => _parse(f(a), index)
          }
        case Plus(l, r) =>
          stack ::= (r, index)
          _parse(l, index)
        case Point(a) => Some((index, Right(a)))
      }

    _parse(parser, index)
  }

  def parse2[I, E, A](
      parser: Parser[I, E, A],
      input: Int => I,
      index: Int
  ): List[(Int, Either[E, A])] = {
    var stack: List[(Parser[I, E, A], Int)] = Nil
    var results: List[(Int, Either[E, A])] = Nil
    def _parse(
        parsers: List[(Int, Parser[I, E, A])]
    ): Unit = {
      var ps: List[(Int, Parser[I, E, A])] = Nil
      parsers.foreach {
        case (i, p) =>
          p match {
            case Derive(d) =>
              ps ::= (index + 1, d(input(index)))
            case Empty    => ()
            case Error(e) => results ::= (index, Left(e))
            case FlatMap(pa, f) =>
              pa match {
                case Derive(d) =>
                  ps ::= (index + 1, d(input(index)).flatMap(f))
                case Empty    => ()
                case Error(e) => results ::= (index, Left(e))
                case fm: FlatMap[I, E, a, b] =>
                  ps ::= (
                    index,
                    fm.pa.flatMap(fm.f(_: a).flatMap(f))
                  )
                case Plus(l, r) =>
                  ps ::= (index, l.flatMap(f))
                  ps ::= (index, r.flatMap(f))
                case Point(a) => ps ::= (index, f(a))
              }
            case Plus(l, r) =>
              ps ::= (index, l)
              ps ::= (index, r)
            case Point(a) => results ::= (index, Right(a))
          }
      }
      if (results.isEmpty && ps.nonEmpty) _parse(ps.reverse)
    }
    _parse(List((index, parser)))
    results.reverse
  }

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
        store
          .getOrElseUpdate(
            (i, p),
            p match {
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
          )
          .asInstanceOf[Option[(Int, Either[E, B])]]

      alternative(parser, _)
    }
  }
}
