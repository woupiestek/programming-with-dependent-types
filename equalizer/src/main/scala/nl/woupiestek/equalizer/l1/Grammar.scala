package nl.woupiestek.equalizer.l1

import scalaz._
import nl.woupiestek.equalizer.parsing.ParserT

class Grammar[F[+ _]: MonadPlus, T, D](
    T: AST.Type[T],
    D: AST.Def[D]
) {

  private type Q[+A] = ParserT[F, Char, String, A]

  private def readIf(f: Char => Boolean): Q[Char] =
    ParserT.readIf(f)
  private def error[A](message: String): Q[A] =
    ParserT.error(message)
  private val pause: Q[Unit] = ParserT.write(())

  def whitespace: Q[Unit] =
    readIf(Character.isWhitespace(_: Char))
      .flatMap((_: Char) => whitespace) ++ pause

  def token(char: Char): Q[Unit] =
    readIf(_ == char).flatMap((_: Char) => whitespace)

  def matchChars(chars: List[Char]): Q[Unit] = chars match {
    case Nil => pause
    case h :: t =>
      readIf(_ == h).flatMap((_: Char) => matchChars(t))
  }

  def matchString(string: String) =
    matchChars(string.toList)

  def token(string: String): Q[Unit] =
    matchString(string).flatMap((_: Unit) => whitespace)

  val identifier: Q[String] = {
    lazy val iPart: Q[List[Char]] =
      (for {
        h: Char <- readIf(
          Character.isJavaIdentifierPart(_: Char)
        )
        t <- iPart
      } yield h :: t) ++ whitespace.map(_ => Nil)

    for {
      h: Char <- readIf(
        Character.isJavaIdentifierStart(_: Char)
      )
      t <- iPart
    } yield (h :: t).mkString
  }

  def separated[A](
      p: => Q[A]
  )(separator: Q[Unit]): Q[List[A]] = {
    def q: Q[List[A]] =
      for {
        h: A <- p
        _: Unit <- separator
        t <- q ++ ParserT.write(List.empty[A])
      } yield h :: t
    q
  }

  def parenthetical[A](p: => Q[A]): Q[A] =
    for {
      _: Unit <- token('(') //
      y: A <- p
      _: Unit <- token(')')
    } yield y

  def tupled[A](p: => Q[A]): Q[List[A]] =
    for {
      _: Unit <- token('<')
      y: List[A] <- separated(
        p ++ error("improper tuple member")
      )(
        token(',')
      )
      _: Unit <- token('>')
    } yield y

  val arrow = token("->")

  val fix = token('@')

  val typeExp: Q[T] = {
    def arrowTail(s: T): Q[T] =
      (for {
        _: Unit <- arrow
        t <- bound
      } yield T.arrow(s, t)) ++
        ParserT.write(s)

    def bound: Q[T] =
      identifier.flatMap { (a: String) =>
        (for {
          _: Unit <- fix
          b <- bound
        } yield T.fix(a, b)) ++
          (for {
            _: Unit <- token('=')
            b: T <- bound
            _: Unit <- token(';')
            c <- bound
          } yield T.let(a, b, c)) ++
          arrowTail(T.variable(a))
      } ++
        (parenthetical(bound) ++
          tupled(bound).map(T.product(_)))
          .flatMap(arrowTail) ++
        error("malformed type")

    bound
  }

  val integer: Q[Int] = {
    val digit: Q[Int] = readIf(Character.isDigit(_: Char))
      .map((c: Char) => c - '0')
    lazy val digits: Q[Int] = for {
      h: Int <- digit
      t: Int <- digits ++ ParserT.write(0)
    } yield 10 * h + t
    digits
  }

  val defExp: Q[D] = {

    def unit: Q[D] =
      parenthetical(intros) ++
        tupled(intros).map(D.tuple(_)) ++
        identifier.map(D.variable(_)) ++
        error("not a delimited def")

    def elim(d: D): Q[D] =
      readIf(_ == '_').flatMap(
        (_: Char) => integer.map(D.project(d, _))
      ) ++
        token('\'').map(_ => D.unfold(d)) ++
        unit.map(D.application(d, _)) ++
        ParserT.write(d)

    def elims(d: D): Q[D] =
      elim(d).flatMap(elims) ++ ParserT.write(d)

    def intros: Q[D] =
      (identifier.flatMap { (a: String) =>
        (for {
          _: Unit <- token('=')
          b: D <- intros
          _: Unit <- token(';')
          c: D <- intros
        } yield D.let(a, b, c)) ++
          arrow.flatMap(
            (_: Unit) => intros.map(D.abstraction(a, _))
          ) ++
          fix.flatMap((_: Unit) => intros.map(D.fix(a, _)))
      }) ++
        unit.flatMap(elims)

    intros
  }
}
