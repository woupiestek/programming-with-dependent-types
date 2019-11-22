package nl.woupiestek.equalizer.l1

import nl.woupiestek.equalizer.parsing.Parser3

object Grammar3 {

  def dropWhile(
      f: Char => Boolean
  )(index: Int)(g: Int => Char): Int = {
    var i = index
    while (f(g(i))) i += 1
    i
  }

  def whitespace: Int => (Int => Char) => Int =
    dropWhile(Character.isWhitespace)

  def token(char: Char): Parser3[Char, Unit] =
    Parser3.unit(
      (f: Int => Char) =>
        (index: Int) =>
          if (f(index) == char) Some(whitespace(index + 1)(f))
          else None
    )

  def token(string: String): Parser3[Char, Unit] =
    Parser3.unit(
      (f: Int => Char) =>
        (index: Int) =>
          if ((0 until string.length())
                .forall(i => string.charAt(i) == f(index + i)))
            Some(whitespace(index + string.length())(f))
          else None
    )

  def identifier: Parser3[Char, String] =
    Parser3(
      (f: Int => Char) =>
        (index: Int) =>
          if (Character.isJavaIdentifierPart(f(index))) {
            val i =
              dropWhile(Character.isJavaIdentifierPart)(
                index + 1
              )(f)
            Some(
              (
                whitespace(i)(f),
                (index until i).map(f).mkString
              )
            )
          } else {
            None
          }
    )

  def integer: Parser3[Char, Int] = Parser3 {
    (f: Int => Char) => (index: Int) =>
      var i = index
      var j = 0
      while (Character.isDigit(f(i))) {
        j = 10 * j + f(i) - '0'
        i += 1
      }
      if (index == i) None
      else Some((whitespace(i)(f), j))
  }

  def expression[D](D: AST.Def[D]): Parser3[Char, D] = {
    lazy val cut: Parser3[Char, D] = (for {
      a: String <- identifier
      _: Unit <- token('=')
      b: D <- intro
      _: Unit <- token(';')
      c: D <- cut
    } yield D.let(a, b, c)) ++
      intro

    lazy val intro: Parser3[Char, D] = (for {
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

    lazy val elim: Parser3[Char, D] = unit.flatMap(
      (a: D) =>
        token('\'').map((_: Unit) => D.unfold(a)) ++
          integer.map(D.project(a, _)) ++
          unit.map(D.application(a, _)) ++
          Parser3.point(a)
    )

    lazy val unit: Parser3[Char, D] = (for {
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

    lazy val tail: Parser3[Char, List[D]] =
      token('>').map((_: Unit) => List.empty[D]) ++ (for {
        _: Unit <- token(',')
        h: D <- cut
        t: List[D] <- tail
      } yield h :: t)

    cut.memoized
  }

}
