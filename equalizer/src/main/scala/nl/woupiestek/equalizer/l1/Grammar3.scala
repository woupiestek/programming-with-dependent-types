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
      while (Character.isJavaIdentifierPart(f(i))) {
        i += 1
        j = 10 * j + f(i) - '0'
      }
      if (index == i) None
      else Some((whitespace(i)(f), j))
  }

  def expression[D](D: AST.Def[D]): Parser3[Char, D] = {

    def iOp(a: String): Parser3[Char, D] = {
      (for {
        _ <- token('=')
        b <- exp
        _ <- token(',')
        c <- exp
      } yield D.let(a, b, c)) ++
        (for {
          _ <- token("->")
          b <- exp
        } yield D.abstraction(a, b)) ++
        (for {
          _ <- token('@')
          b <- exp
        } yield D.fix(a, b))
    }

    def dOp(d: D): Parser3[Char, D] =
      Parser3.point[Char, D](d) ++ (
        token('\'')
          .flatMap(_ => exp.map(D.unfold)) ++
          token('.')
            .flatMap(_ => integer.map(D.project(d, _))) ++
          exp.map(D.application(d, _))
      ).flatMap(dOp)

    def tOp: Parser3[Char, List[D]] =
      exp.flatMap(
        (h: D) =>
          token('>')
            .map((_: Unit) => List(h)) ++
            token(',')
              .flatMap(
                (_: Unit) => tOp.map(h :: (_: List[D]))
              )
      )

    lazy val exp: Parser3[Char, D] = {
      identifier.flatMap(
        (s: String) => dOp(D.variable(s)) ++ iOp(s)
      ) ++ (for {
        _: Unit <- token('(')
        d: D <- exp
        _: Unit <- token(')')
      } yield d) ++ (for {
        _: Unit <- token('<')
        ds: List[D] <- (tOp ++
          token('>')
            .map((_: Unit) => List.empty[D]))
      } yield D.tuple(ds))
    }.memoized

    exp
  }

}
