package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.Parser2
import nl.woupiestek.equalizer.parsing.Parser2._

class Grammar2[D](
    D: AST.Def[D]
) {

  private type Q[+A] = Parser2[Char, String, A]

  private def select[A](
      f: Char => Boolean
  )(q: Char => Q[A]): Q[A] =
    Parser2.read(
      (c: Char) => if (f(c)) q(c) else Parser2.empty
    )

  private def on[A](f: Char => Boolean)(qa: => Q[A]): Q[A] =
    select(f)((_: Char) => qa)

  private val pause: Q[Unit] = Parser2.point(())

  lazy val whitespace: Q[Unit] =
    on(Character.isWhitespace(_: Char))(whitespace) ++ pause

  private def onToken[A](token: String)(
      parser: => Q[A]
  ): Q[A] = {
    def helper(chars: List[Char]): Q[A] = chars match {
      case Nil    => whitespace *> parser
      case h :: t => on((_: Char) == h)(helper(t))
    }

    helper(token.toList)
  }

  val identifier: Q[String] = {
    lazy val iPart: Q[List[Char]] =
      select(Character.isJavaIdentifierPart(_: Char))(
        (h: Char) => iPart.map(h :: (_: List[Char]))
      ) ++ Parser2.point(Nil)

    select(Character.isJavaIdentifierStart(_: Char))(
      (h: Char) => iPart.map(h :: (_: List[Char]))
    ).map((_: List[Char]).mkString) <* whitespace
  }

  val integer: Q[Int] = {
    lazy val digits: Q[List[Int]] =
      select(Character.isDigit((_: Char)))(
        (c: Char) =>
          digits.map((t: List[Int]) => (c - '0') :: t) ++
            Parser2.point(List.empty[Int])
      )

    digits.map(
      (_: List[Int]).foldLeft(0)((a, d) => 10 * a + d)
    )
  }

  val defExp: Q[D] = {

    lazy val iOp: Q[String => D] = {
      (onToken("=")(expression) |@|
        onToken(";")(expression))(
        (b: D, c: D) => (a: String) => D.let(a, b, c)
      ) ++
        onToken("->")(expression).map(
          (b: D) => (a: String) => D.abstraction(a, b)
        ) ++
        onToken("@")(expression).map(
          (b: D) => (a: String) => D.fix(a, b)
        )
    }

    lazy val dOp: Q[D => D] = {
      val op: Q[D => D] =
        onToken("'")(Parser2.point(D.unfold(_))) ++
          onToken("_")(
            integer.map((i: Int) => D.project(_: D, i)) <* whitespace
          ) ++
          expression.map((e: D) => D.application(_: D, e))

      Parser2.point((d: D) => d) ++ (op |@| dOp)(
        (f: D => D, g: D => D) => (d: D) => g(f(d))
      )
    }

    lazy val tOp: Q[List[D]] =
      (expression |@| ((onToken(">")(Parser2.point(Nil)) ++
        onToken(",")(tOp))))(_ :: _)

    lazy val expression: Q[D] = {
      identifier.ap(
        dOp.map((_: D => D).compose(D.variable(_))) ++ iOp
      ) ++
        onToken("(")(expression <* onToken(")")(pause)) ++
        onToken("<")(
          tOp ++ onToken(">")(Parser2.point(Nil))
        ).map(D.tuple(_))
    }

    expression
  }
}
