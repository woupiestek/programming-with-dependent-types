package nl.woupiestek.equalizer.l1

import nl.woupiestek.equalizer.parsing.Parser

class Grammar[T, D](
    T: AST.Type[T],
    D: AST.Def[D]
) {

  private type Q[+A] = Parser[Char, String, A]

  private def readIf(f: Char => Boolean): Q[Char] =
    Parser.read.filter(f)
  private def error[A](message: String): Q[A] =
    Parser.error(message)
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
      } yield h :: t) ++ whitespace.map((_: Unit) => Nil)

    for {
      h: Char <- readIf(
        Character.isJavaIdentifierStart(_: Char)
      )
      t: List[Char] <- iPart
    } yield (h :: t).mkString
  }

  def parenthetical[A](p: => Q[A]): Q[A] =
    for {
      _: Unit <- token('(') //
      y: A <- p
      _: Unit <- token(')')
    } yield y

  def tupled[A](p: Q[A]): Q[List[A]] = {
    def q: Char => Q[List[A]] = {
      case '>' => whitespace.map((_: Unit) => Nil)
      case ',' =>
        for {
          _: Unit <- whitespace
          h: A <- p
          c: Char <- Parser.read
          t: List[A] <- q(c)
        } yield h :: t
      case _ => Parser.empty
    }

    for {
      _: Unit <- token('<')
      h: A <- p
      c: Char <- Parser.read
      t: List[A] <- q(c)
    } yield h :: t
  }

  val arrow = token("->")

  val fix = token('@')

  val typeExp: Q[T] = {
    def arrowTail(s: T): Q[T] =
      (for {
        _: Unit <- arrow
        t <- bound
      } yield T.arrow(s, t)) ++
        Parser.point(s)

    lazy val bound: Q[T] =
      Parser.suspend(
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
      )

    bound
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

    def unit: Q[D] =
      parenthetical(intros) ++
        tupled(intros).map(D.tuple(_)) ++
        identifier.map(D.variable(_)) ++
        error("not a delimited def")

    def dOps(d: D): Char => Q[D] = {
      case '_'  => integer.map(D.project(d, _))
      case '\'' => Parser.point(D.unfold(d))
      case _    => Parser.empty
    }

    def elims(d: D): Q[D] =
      (Parser.read.flatMap(dOps(d)) ++
        unit.map(D.application(d, _)))
        .flatMap(elims) ++ Parser.point(d)

    def idOps(a: String): Char => Q[D] = {
      case '=' =>
        for {
          _: Unit <- whitespace
          b: D <- intros
          _: Unit <- token(';')
          c: D <- intros
        } yield D.let(a, b, c)
      case '-' =>
        for {
          _: Char <- readIf(_ == '>')
          _: Unit <- whitespace
          b: D <- intros
        } yield D.abstraction(a, b)
      case '@' =>
        for {
          _: Unit <- whitespace
          b: D <- intros
        } yield D.fix(a, b)
      case _ => Parser.empty
    }

    def intros: Q[D] =
      Parser.suspend(
        (for {
          a: String <- identifier
          chr: Char <- Parser.read[Char, String]
          d: D <- idOps(a)(chr)
        } yield d) ++
          unit.flatMap(elims)
      )
    intros
  }
}
