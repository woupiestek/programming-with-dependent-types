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
    (readIf(Character.isWhitespace(_: Char)) *> whitespace) <+> ()
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
      (readIf(Character.isJavaIdentifierPart(_: Char)) <*> iPart)
        .map((t: List[Char]) => (_: Char) :: t) <+>
        whitespace.map(_ => (_: Char) :: Nil)
    (readIf(Character.isJavaIdentifierStart(_: Char)) <*> iPart)
      .map(_.mkString) <* whitespace
  }

  def separated[A](
      p: Q[A]
  )(separator: Q[Unit]): Q[List[A]] = {
    lazy val tail: Q[List[A]] =
      (p <*> separator *> tail.map(
        (t: List[A]) => (_: A) :: t
      )) <+>
        List.empty[A].point[Q]
    tail
  }

  def parenthetical[A](p: Q[A]) =
    token('(') *> p <* token(')') //

  def tupled[A](p: Q[A]): Q[List[A]] =
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

    lazy val bound: Q[T] =
      identifier <*> {
        (fix *> bound.map(c => T.fix(_, c))) <+>
          (token('=') *> bound <* token(';') <*> bound.map(
            (c: T) => (b: T) => T.let(_, b, c)
          )) <+>
          arrowTail.map((_: T => T).compose(T.variable(_)))
      } <+>
        ({
          parenthetical(bound) <+>
            tupled(bound).map(T.product(_))
        } <*> arrowTail) <+>
        error("malformed type")

    bound
  }

  val integer: Q[Int] = {
    lazy val digits: Q[Int => Int] =
      (readIf(Character.isDigit(_: Char))
        .map((c: Char) => (acc: Int) => 10 * acc + c - '0') <*> digits
        .map((f: Int => Int) => f.compose(_: Int => Int))) <+>
        ((i: Int) => i).point[Q]
    digits.map(_(0))
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
      elim <*> elims
        .map((f: D => D) => f.compose(_: D => D)) <+>
        ((d: D) => d).point[Q]

    def intros: Q[D] =
      (identifier <*> {
        (token('=') *> (intros <*> token(';') *> intros.map(
          (c: D) => (b: D) => D.let(_, b, c)
        ))) <+>
          (arrow *> intros.map(
            (b: D) => D.abstraction(_, b)
          )) <+>
          (fix *> intros.map((b: D) => D.fix(_, b)))
      }) <+>
        (unit <*> elims)

    intros
  }
}
