package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.Parser3
import nl.woupiestek.equalizer.parsing.Parser3._

class Grammar3[D](
    D: AST.Def[D]
) {

  private type Q[+A] = Parser3[Char, String, A]

  private val pause: Q[Unit] = Parser3.point(())
  private def readIf(f: Char => Boolean): Q[Unit] =
    Parser3.readIf(f, pause)

  val whitespace: Q[Unit] =
    Parser3.loop(
      (ws: Q[Unit]) =>
        (readIf(Character.isWhitespace(_: Char)) *>
          ws) ++ pause
    )

  def token(char: Char): Q[Unit] =
    readIf(_ == char) *> whitespace

  def matchChars(chars: List[Char]): Q[Unit] = chars match {
    case Nil    => pause
    case h :: t => readIf(_ == h) *> matchChars(t)
  }

  def matchString(string: String) =
    matchChars(string.toList)

  def token(string: String): Q[Unit] =
    matchString(string) *> whitespace

  private def foldMap[A](
      seq: Seq[Char]
  )(f: Char => Q[A]): Q[A] =
    seq.foldLeft[Q[A]](Parser3.empty)(
      (p, c) => p ++ f(c)
    )

  val identifier: Q[String] = {
    val start = ('a' to 'z') ++ ('A' to 'Z')
    val part = start ++ ('0' to '9')
    val tail = Parser3.loop(
      (iPart: Q[List[Char]]) =>
        foldMap(part)(
          h =>
            for {
              _: Unit <- readIf((_: Char) == h)
              t: List[Char] <- iPart
            } yield h :: t
        ) ++ (Parser3.point(List.empty[Char]))
    )

    foldMap(start)(
      h =>
        for {
          _: Unit <- readIf((_: Char) == h)
          t: List[Char] <- tail
          _: Unit <- whitespace
        } yield (h :: t).mkString
    )
  }

  val integer: Q[Int] = {
    val digit: Q[Int] =
      foldMap('0' to '9')(
        (d: Char) =>
          readIf((_: Char) == d)
            .map((_: Unit) => d - '0')
      )
    Parser3.loop(
      (digits: Q[Int]) =>
        for {
          h: Int <- digit
          t: Int <- digits ++ Parser3.point(0)
        } yield 10 * h + t
    )
  }

  val defExp: Q[D] = {

    def iOp(a: String, expression: Q[D]): Q[D] = {
      (for {
        _: Unit <- token('=')
        b: D <- expression
        _: Unit <- token(';')
        c: D <- expression
      } yield D.let(a, b, c)) ++
        (for {
          _: Unit <- token("->")
          b: D <- expression
        } yield D.abstraction(a, b)) ++
        (for {
          _: Unit <- token('@')
          b: D <- expression
        } yield D.fix(a, b))
    }

    def dOp(d: D, expression: Q[D]): Q[D] = {
      Parser3.point(d) ++
        (token('\'').map((_: Unit) => D.unfold(d)) ++
          (for {
            i: Int <- integer
            _: Unit <- whitespace
          } yield D.project(d, i)) ++
          expression.map(D.application(d, _)))
          .flatMap(dOp(_, expression))
    }

    def tOp(expression: Q[D]): Q[List[D]] =
      expression.flatMap(
        (h: D) =>
          token('>').map((_: Unit) => List(h)) ++
            (token(',') *>
              tOp(expression).map(h :: (_: List[D])))
      )

    Parser3.loop { (expression: Q[D]) =>
      identifier.flatMap(
        (s: String) =>
          dOp(D.variable(s), expression) ++ iOp(
            s,
            expression
          )
      ) ++ (for {
        _: Unit <- token('(') //
        d: D <- expression
        _: Unit <- token(')')
      } yield d) ++ (for {
        _: Unit <- token('<')
        ds: List[D] <- (tOp(expression) ++
          token('>').map((_: Unit) => List.empty[D]))
      } yield D.tuple(ds))
    }
  }
}
