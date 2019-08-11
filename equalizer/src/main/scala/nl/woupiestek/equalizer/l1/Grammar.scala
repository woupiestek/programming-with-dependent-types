package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.ParserT
import ParserT._

class Grammar[F[+ _]: MonadPlus, T, D](
    T: AST.Type[T],
    D: AST.Def[D]
) {

  private type Q[+A] = ParserT[F, Char, String, A]

  private def readIf(f: Char => Boolean): Q[Char] =
    ParserT.readIf(f)
  private def error[A](message: String): Q[A] =
    ParserT.error(message)

  lazy val whitespace: Q[Unit] =
    (readIf(Character.isWhitespace(_: Char))
      .flatMap(_ => whitespace)) <+> ()
      .point[Q]

  def token(char: Char): Q[Unit] =
    readIf(_ == char) *> whitespace

  def matchChars(chars: List[Char]): Q[Unit] = chars match {
    case Nil    => ().point[Q]
    case h :: t => readIf(_ == h) *> matchChars(t)
  }

  def matchString(string: String) =
    matchChars(string.toList)

  def token(string: String): Q[Unit] =
    matchString(string) *> whitespace

  val identifier: Q[String] = {
    lazy val iPart: Q[Char => List[Char]] =
      readIf(Character.isJavaIdentifierPart(_: Char))
        .flatMap(
          (h1: Char) =>
            iPart.map(
              (t: Char => List[Char]) =>
                (h0: Char) => h0 :: t(h1)
            )
        ) <+>
        whitespace.map(_ => (_: Char) :: Nil)
    (readIf(Character.isJavaIdentifierStart(_: Char)) <*> iPart)
      .map(_.mkString)
  }

  def separated[A](
      p: => Q[A]
  )(separator: Q[Unit]): Q[List[A]] = {
    lazy val q: Q[List[A]] = for {
      h <- p
      _ <- separator
      t <- q <+> List.empty[A].point[Q]
    } yield h :: t
    q
  }

  def parenthetical[A](p: => Q[A]) =
    token('(') *> p <* token(')') //

  def tupled[A](p: => Q[A]): Q[List[A]] =
    token('<') *>
      separated(p <+> error("improper tuple member"))(
        token(',')
      ) <*
      token('>')

  val arrow = token("->")

  val fix = token('@')

  val typeExp: Q[T] = {
    lazy val arrowTail: Q[T => T] =
      (arrow *> bound.map((t: T) => T.arrow(_, t))) <+>
        ((t: T) => t).point[Q]

    def bound: Q[T] =
      identifier.flatMap { (a: String) =>
        (fix *> bound.map(T.fix(a, _))) <+>
          (token('=') *> bound <* token(';') <*> bound.map(
            (c: T) => (b: T) => T.let(a, b, c)
          )) <+>
          arrowTail.map((_: T => T)(T.variable(a)))
      } <+>
        ({
          parenthetical(bound) <+>
            tupled(bound).map(T.product(_))
        } <*> arrowTail) <+>
        error("malformed type")

    bound
  }

  val integer: Q[Int] = {
    val digit: Q[Int] = readIf(Character.isDigit(_: Char))
      .map((c: Char) => c - '0')
    lazy val digits: Q[Int] = for {
      h <- digit
      t <- digits <+> 0.point[Q]
    } yield 10 * h + t
    digits
  }

  val defExp: Q[D] = {

    def unit: Q[D] =
      parenthetical(intros) <+>
        tupled(intros).map(D.tuple(_)) <+>
        identifier.map(D.variable(_)) <+>
        error("not a delimited def")

    def elim: Q[D => D] =
      (readIf(_ == '_') *> integer.map(
        (i: Int) => D.project(_, i)
      )) <+>
        token('\'').map(_ => D.unfold(_)) <+>
        unit.map((operand: D) => D.application(_, operand)) <+>
        ((d: D) => d).point[Q]

    def elims: Q[D => D] =
      (for {
        f <- elim
        g <- elims
      } yield f.andThen(g)) <+>
        ((d: D) => d).point[Q]

    def intros: Q[D] =
      (identifier.flatMap { (a: String) =>
        (token('=') *> (intros <*> token(';') *> intros.map(
          (c: D) => (b: D) => D.let(a, b, c)
        ))) <+>
          (arrow *> intros.map(
            (b: D) => D.abstraction(a, b)
          )) <+>
          (fix *> intros.map((b: D) => D.fix(a, b)))
      }) <+>
        (unit <*> elims)

    intros
  }
}
