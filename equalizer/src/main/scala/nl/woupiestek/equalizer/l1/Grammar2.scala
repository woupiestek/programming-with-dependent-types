package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.Parser2
import nl.woupiestek.equalizer.parsing.Parser2._

object Grammar2 {

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

  def onIdentifier[A](f: String => Q[A]): Q[A] = {
    def iPart(chars: List[Char]): Q[A] =
      select(Character.isJavaIdentifierPart(_: Char))(
        (h: Char) => iPart(h :: chars)
      ) ++ (whitespace *> f(chars.reverse.mkString))

    select(Character.isJavaIdentifierStart(_: Char))(
      (h: Char) => iPart(h :: Nil)
    )
  }

  def onIndex[A](f: Int => Q[A]): Q[A] = {
    def digits(index: Int): Q[A] =
      select(Character.isDigit((_: Char))) { (c: Char) =>
        val j = 10 * index + c - '0'
        digits(j) ++ (whitespace *> f(j))
      }

    digits(0)
  }

  def defExp[D](D: AST.Def[D]): Q[D] = {

    def iOp[A](a: String, f: D => Q[A]): Q[A] = {
      onToken("=")(
        onExpression(
          (b: D) =>
            onToken(";")(
              onExpression(
                (c: D) => f(D.let(a, b, c))
              )
            )
        )
      ) ++
        onToken("->")(
          onExpression(
            (b: D) => f(D.abstraction(a, b))
          )
        ) ++
        onToken("@")(
          onExpression((b: D) => f(D.fix(a, b)))
        )
    }

    def dOp[A](d: D, f: D => Q[A]): Q[A] = {
      onToken("'")(dOp(D.unfold(d), f)) ++
        onIndex((i: Int) => dOp(D.project(d, i), f)) ++
        onExpression((e: D) => dOp(D.application(d, e), f)) ++
        f(d)
    }

    def tOp[A](stack: List[D], f: List[D] => Q[A]): Q[A] =
      onExpression(
        (h: D) =>
          onToken(">")(f((h :: stack).reverse)) ++
            onToken(",")(tOp(h :: stack, f))
      )

    def onExpression[A](f: D => Q[A]): Q[A] = {
      onIdentifier(
        (name: String) =>
          dOp(D.variable(name), f)
            ++ iOp(name, f)
      ) ++
        onToken("(")(
          onExpression(
            (d: D) => onToken(")")(f(d))
          )
        ) ++
        onToken("<")(
          tOp(
            List.empty[D],
            (ds: List[D]) => f(D.tuple(ds))
          ) ++ onToken(">")(f(D.tuple(Nil)))
        )
    }

    onExpression(Parser2.point(_: D))
  }
}
