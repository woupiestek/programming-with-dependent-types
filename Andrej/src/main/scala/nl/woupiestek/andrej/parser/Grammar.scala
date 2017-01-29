package nl.woupiestek.andrej.parser

import nl.woupiestek.andrej.parser.Grammar.{Fail, Point, Read}

sealed trait Grammar[-In, +Out] {
  def |[In2 <: In, Out2 >: Out](grammar: Grammar[In2, Out2]): Grammar[In2, Out2]

  def flatMap[In2 <: In, Out2](f: Out => Grammar[In2, Out2]): Grammar[In2, Out2]

  def map[Out2](f: Out => Out2): Grammar[In, Out2] = flatMap((out: Out) => Point(f(out)))

  def withFilter(f: Out => Boolean): Grammar[In, Out] = flatMap((out: Out) => if (f(out)) Point(out) else Fail)

  def filter(f: Out => Boolean): Grammar[In, Out] = withFilter(f)

  def collect[Out2](f: PartialFunction[Out, Out2]): Grammar[In, Out2] =
    flatMap((out: Out) => if (f.isDefinedAt(out)) Point(f(out)) else Fail)

  def zeroOrMore: Grammar[In, List[Out]] = oneOrMore | Point(Nil)

  def oneOrMore: Grammar[In, List[Out]] = for {
    x <- this
    y <- zeroOrMore
  } yield x :: y

  def zeroOrOne: Grammar[In, Option[Out]] = map(Some(_)) | Point(None)

  def andThen[Out2](grammar: Grammar[Out, Out2]): Grammar[In, Out2] = grammar match {
    case Fail => Fail
    case Point(out, alternative) => Point(out, andThen(alternative))
    case Read(read, alternative) => flatMap(out => andThen(read(out))) | andThen(alternative)
  }
}

object Grammar {

  case object Fail extends Grammar[Any, Nothing] {
    override def flatMap[In2 <: Any, Out2](f: (Nothing) => Grammar[In2, Out2]): Grammar[In2, Out2] = Fail

    override def |[In2 <: Any, Out2 >: Nothing](grammar: Grammar[In2, Out2]): Grammar[In2, Out2] = grammar
  }

  case class Point[-In, +Out](out: Out, alternative: Grammar[In, Out] = Fail) extends Grammar[In, Out] {
    override def flatMap[In2 <: In, Out2](f: (Out) => Grammar[In2, Out2]): Grammar[In2, Out2] =
      f(out) | (alternative flatMap f)

    override def |[In2 <: In, Out2 >: Out](grammar: Grammar[In2, Out2]): Grammar[In2, Out2] =
      Point(out, alternative | grammar)

  }

  case class Read[-In, +Out](read: In => Grammar[In, Out], alternative: Grammar[In, Out] = Fail) extends Grammar[In, Out] {
    override def |[In2 <: In, Out2 >: Out](grammar: Grammar[In2, Out2]): Grammar[In2, Out2] =
      Read(read, alternative | grammar)

    override def flatMap[In2 <: In, Out2](f: (Out) => Grammar[In2, Out2]): Grammar[In2, Out2] =
      Read((in: In) => read(in).flatMap(f), alternative.flatMap(f))
  }

  def read[In]:Grammar[In,In] = Read(Point(_))

  def collect[In, Out](f: PartialFunction[In, Out]): Grammar[In, Out] = Read {
    case x if f.isDefinedAt(x) => Point(f(x))
    case _ => Fail
  }
}
