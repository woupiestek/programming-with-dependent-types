package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.Parser2
import nl.woupiestek.equalizer.parsing.Parser2._

class Grammar2[D](
    D: AST.Def[D]
) {

  private type Q[+A] = Parser2[Char, String, A]

  private def readIf(f: Char => Boolean): Q[Char] =
    Parser2.readIf(f)

  private val pause: Q[Unit] = Parser2.point(())

  def whitespace: Q[Unit] =
    Parser2.suspend(
      (readIf(Character.isWhitespace(_: Char)) *>
        whitespace)
    ) ++ pause

  def token(char: Char): Q[Unit] =
    readIf(_ == char) *> whitespace

  def matchChars(chars: List[Char]): Q[Unit] =
    chars match {
      case Nil => pause
      case h :: t =>
        Parser2.suspend(readIf(_ == h) *> matchChars(t))
    }

  def matchString(string: String) =
    matchChars(string.toList)

  def token(string: String): Q[Unit] =
    matchString(string) *> whitespace

  val identifier: Q[String] = {
    def iPart: Q[List[Char]] =
      Parser2.suspend(
        (readIf(
          Character.isJavaIdentifierPart(_: Char)
        ) |@| iPart)(_ :: _)
      ) ++ Parser2
        .point(Nil)

    (readIf(
      Character.isJavaIdentifierStart(_: Char)
    ) |@| iPart)(_ :: _)
      .map((_: List[Char]).mkString) <* whitespace
  }

  val integer: Q[Int] = {
    val digit: Q[Int] = readIf(Character.isDigit(_: Char))
      .map((c: Char) => c - '0')
    def digits: Q[Int] = Parser2.suspend(
      (digit |@|
        digits)((h: Int, t: Int) => 10 * h + t)
        ++ Parser2
          .point(0)
    )

    digits
  }

  val defExp: Q[D] = {

    def iOp: Q[String => D] = Parser2.suspend {
      ((token('=') *> expression) |@|
        (token(';') *> expression))(
        (b: D, c: D) => (a: String) => D.let(a, b, c)
      ) ++
        (token("->") *> expression.map(
          (b: D) => (a: String) => D.abstraction(a, b)
        )) ++
        (token('@') *> expression.map(
          (b: D) => (a: String) => D.fix(a, b)
        ))
    }

    def dOp: Q[D => D] = {
      val op: Q[D => D] =
        (token('\'') *> Parser2.point(D.unfold(_))) ++
          (integer.map((i: Int) => D.project(_: D, i)) <* whitespace) ++
          expression.map((e: D) => D.application(_: D, e))

      Parser2.point((d: D) => d) ++
        Parser2.suspend(
          (op |@| dOp)(
            (f: D => D, g: D => D) => (d: D) => g(f(d))
          )
        )
    }

    def tOp: Q[List[D]] =
      (expression |@| (
        (token('>') *> Parser2.point(Nil)) ++
          Parser2.suspend(token(',') *> tOp)
      ))(_ :: _)

    def expression: Q[D] = {
      identifier.ap(
        dOp.map((_: D => D).compose(D.variable(_))) ++ iOp
      ) ++ {
        Parser2.suspend(
          token('(') //
            *> expression <* token(')')
        )
      } ++ {
        token('<') *> (tOp ++
          (token('>') *> Parser2.point(Nil)))
          .map(D.tuple(_))
      }
    }

    expression
  }
}
