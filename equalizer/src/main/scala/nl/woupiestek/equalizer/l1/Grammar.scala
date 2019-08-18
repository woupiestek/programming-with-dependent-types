package nl.woupiestek.equalizer.l1

import nl.woupiestek.equalizer.parsing.Parser

class Grammar[D](
    D: AST.Def[D]
) {

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
    val digit: Q[Int] = readIf(Character.isDigit(_: Char))
      .map((c: Char) => c - '0')
    lazy val digits: Q[Int] = for {
      h: Int <- digit
      t: Int <- digits ++ Parser.point(0)
    } yield 10 * h + t
    digits
  }

  val defExp: Q[D] = {

    def iOp(a: String): Q[D] = {
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

    def dOp(d: D): Q[D] = {
      Parser.point(d) ++
        (token('\'').map((_: Unit) => D.unfold(d)) ++
          (for {
            i: Int <- integer
            _: Unit <- whitespace
          } yield D.project(d, i)) ++
          expression.map(D.application(d, _)))
          .flatMap(dOp(_))
    }

    def tOp: Q[List[D]] = {
      expression.flatMap(
        (h: D) =>
          token('>').map((_: Unit) => List(h)) ++
            token(',').flatMap(
              (_: Unit) => tOp.map(h :: (_: List[D]))
            )
      )
    }

    lazy val expression: Q[D] = {
      identifier.flatMap(
        (s: String) => dOp(D.variable(s)) ++ iOp(s)
      ) ++ (for {
        _: Unit <- token('(') //
        d: D <- expression
        _: Unit <- token(')')
      } yield d) ++ (for {
        _: Unit <- token('<')
        ds: List[D] <- (tOp ++
          token('>').map((_: Unit) => List.empty[D]))
      } yield D.tuple(ds))
    }

    expression
  }
}
