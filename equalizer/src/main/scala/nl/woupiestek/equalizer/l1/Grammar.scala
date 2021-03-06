package nl.woupiestek.equalizer.l1

import nl.woupiestek.equalizer.parsing.Parser

object Grammar {

  private type Q[+A] = Parser[Char, String, A]

  private def readIf(f: Char => Boolean): Q[Char] =
    Parser.read.filter(f)

  private val pause: Q[Unit] = Parser.point(())

  lazy val whitespace: Q[Unit] =
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
        t: List[Char] <- iPart
      } yield h :: t) ++ Parser.point(Nil)

    for {
      h: Char <- readIf(
        Character.isJavaIdentifierStart(_: Char)
      )
      t: List[Char] <- iPart
      _: Unit <- whitespace
    } yield (h :: t).mkString
  }

  val integer: Q[Int] = {
    def digits(x: Int): Q[Int] =
      readIf(Character.isDigit(_: Char)).flatMap { (c: Char) =>
        val y = 10 * x + c - '0'
        digits(y) ++ Parser.point(y)
      }

    digits(0)
  }

  def expression[D](D: AST.Def[D]): Q[D] = {
    lazy val cut: Q[D] = (for {
      a: String <- identifier
      _: Unit <- token('=')
      b: D <- intro
      _: Unit <- token(';')
      c: D <- cut
    } yield D.let(a, b, c)) ++
      intro

    lazy val intro: Q[D] = (for {
      a: String <- identifier
      _: Unit <- token('@')
      b: D <- intro
    } yield D.fix(a, b)) ++
      (for {
        a: String <- identifier
        _: Unit <- token("->")
        b: D <- intro
      } yield D.abstraction(a, b)) ++
      elim

    lazy val elim: Q[D] = unit.flatMap(
      (a: D) =>
        token('\'').map((_: Unit) => D.unfold(a)) ++
          integer.map(D.project(a, _)) ++
          unit.map(D.application(a, _)) ++
          Parser.point(a)
    )

    lazy val unit: Q[D] = (for {
      _: Unit <- token('(')
      a: D <- cut
      _: Unit <- token(')')
    } yield a) ++
      token('<').flatMap(
        (_: Unit) =>
          token('>').map((_: Unit) => D.tuple(Nil)) ++
            (for {
              h: D <- cut
              t: List[D] <- tail
            } yield D.tuple(h :: t))
      ) ++
      identifier.map(D.variable)

    lazy val tail: Q[List[D]] =
      token('>').map((_: Unit) => List.empty[D]) ++ (for {
        _: Unit <- token(',')
        h: D <- cut
        t: List[D] <- tail
      } yield h :: t)

    cut
  }
}
