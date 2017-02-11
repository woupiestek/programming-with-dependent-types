package nl.woupiestek.andrej.parser

import nl.woupiestek.andrej.parser.Rule.{ Fail, Point, Read }

import scala.annotation.tailrec

//todo test
sealed trait Rule[-In, +Out] {
  def |[In2 <: In, Out2 >: Out](grammar: Rule[In2, Out2]): Rule[In2, Out2]

  def flatMap[In2 <: In, Out2](f: Out => Rule[In2, Out2]): Rule[In2, Out2]

  def ~[In2 <: In, Out2](f: => Rule[In2, Out2]): Rule[In2, Out2] = flatMap(_ => f)

  def map[Out2](f: Out => Out2): Rule[In, Out2] = flatMap((out: Out) => Point(f(out)))

  def withFilter(f: Out => Boolean): Rule[In, Out] = flatMap((out: Out) => if (f(out)) Point(out) else Fail)

  def filter(f: Out => Boolean): Rule[In, Out] = withFilter(f)

  def collect[Out2](f: PartialFunction[Out, Out2]): Rule[In, Out2] =
    flatMap((out: Out) => if (f.isDefinedAt(out)) Point(f(out)) else Fail)

  def zeroOrMore: Rule[In, List[Out]] = oneOrMore | Point(Nil)

  def oneOrMore: Rule[In, List[Out]] = for {
    x <- this
    y <- zeroOrMore
  } yield x :: y

  def zeroOrOne: Rule[In, Option[Out]] = map(Some(_)) | Point(None)

  def andThen[Out2](grammar: Rule[Out, Out2]): Rule[In, Out2] = grammar match {
    case Fail => Fail
    case Point(out, alternative) => Point(out, andThen(alternative))
    case Read(read, alternative) => flatMap(out => andThen(read(out))) | andThen(alternative)
  }
}

object Rule {

  case object Fail extends Rule[Any, Nothing] {
    override def flatMap[In2 <: Any, Out2](f: (Nothing) => Rule[In2, Out2]): Rule[In2, Out2] = Fail

    override def |[In2 <: Any, Out2 >: Nothing](grammar: Rule[In2, Out2]): Rule[In2, Out2] = grammar
  }

  case class Point[-In, +Out](out: Out, alternative: Rule[In, Out] = Fail) extends Rule[In, Out] {
    override def flatMap[In2 <: In, Out2](f: (Out) => Rule[In2, Out2]): Rule[In2, Out2] =
      f(out) | (alternative flatMap f)

    override def |[In2 <: In, Out2 >: Out](grammar: Rule[In2, Out2]): Rule[In2, Out2] =
      Point(out, alternative | grammar)

  }

  case class Read[-In, +Out](read: In => Rule[In, Out], alternative: Rule[In, Out] = Fail) extends Rule[In, Out] {
    override def |[In2 <: In, Out2 >: Out](grammar: Rule[In2, Out2]): Rule[In2, Out2] =
      Read(read, alternative | grammar)

    override def flatMap[In2 <: In, Out2](f: (Out) => Rule[In2, Out2]): Rule[In2, Out2] =
      Read((in: In) => read(in).flatMap(f), alternative.flatMap(f))
  }

  def read[In]: Rule[In, In] = Read(Point(_))

  def collect[In, Out](f: PartialFunction[In, Out]): Rule[In, Out] = Read {
    case x if f.isDefinedAt(x) => Point(f(x))
    case _ => Fail
  }

  def filter[In](f: In => Boolean): Rule[In, In] = Read(x => if (f(x)) Point(x) else Fail)

  def matchList[I](list: List[I]): Rule[I, Unit] = list match {
    case Nil => Point[I, Unit](())
    case h :: t => for {
      h2 <- read[I] if h2 == h
      _ <- matchList(t)
    } yield ()
  }
}

object StringParser {

  def parse[E](rule: Rule[Option[Char], E], string: String): Either[Int, E] = {
    val cs = string.toCharArray.toIndexedSeq
    type R = Rule[Option[Char], E]

    @tailrec def p(index: Int, rule: R, stack: List[(Int, R)], max: Int): Either[Int, E] = rule match {
      case Fail => stack match {
        case Nil => Left(max)
        case (i, r) :: t => p(i, r, t, max)
      }
      case Read(x, y) => p(index + 1, x(cs lift index), (index, y) :: stack, math.max(max, index))
      case Point(x, y) => if (index == string.length) Right(x) else p(index, y, stack, max)
    }

    p(0, rule, Nil, 0)
  }

}
