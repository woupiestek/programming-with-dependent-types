package nl.woupiestek.andrej.contextfreegrammars

import scala.annotation.tailrec

case class Parser[A, +B](step: Option[A => Parser[A, B]], halt: Option[B]) {
  def join[C >: B](parser: Parser[A, C]): Parser[A, C] = {
    val h = for {
      f <- step
      g <- parser.step
    } yield { a: A => f(a) join g(a) }
    Parser(h, halt orElse parser.halt)
  }

  def flatMap[C](f: B => Parser[A, C]): Parser[A, C] = {
    val g = halt.map(f)
    val h = for {
      x <- g
      y <- x.halt
    } yield y
    val s = for {
      x <- g
      y <- x.step
      z <- step
    } yield { a: A => y(a) join z(a).flatMap(f) }
    Parser(s, h)
  }

  def map[C](f: B => C): Parser[A, C] = {
    val s = for (x <- step) yield { a: A => x(a).map(f) }
    val h = for (x <- halt) yield f(x)
    Parser(s, h)
  }

  def parse(input: List[A]): Option[B] = input match {
    case Nil => halt
    case head :: tail => for {
      x <- step
      y <- x(head).parse(tail)
    } yield y
  }
}

object Parser {
  @tailrec def parse[A, B](parser: Parser[A, B], input: List[A]): Option[B] = input match {
    case Nil => parser.halt
    case head :: tail => parser.step match {
      case Some(f) => parse(f(head), tail)
      case None => None
    }
  }
}