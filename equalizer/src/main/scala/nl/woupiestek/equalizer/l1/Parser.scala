package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._

abstract class Parser[P[_]: ApplicativePlus, T, D](
    T: AST.Type[T],
    D: AST.Def[D]
) {

  def readIf(f: Char => Boolean): P[Char]
  def error[A](message: String): P[A]

  lazy val whitespace: P[Unit] =
    (readIf(Character.isWhitespace(_: Char)) *> whitespace) <+> ().point[P]

  def token(char: Char): P[Unit] = readIf(_ == char) *> whitespace

  def matchChars(chars: List[Char]): P[Unit] = chars match {
    case Nil    => ().point[P]
    case h :: t => readIf(_ == h) *> matchChars(t)
  }

  def matchString(string: String) = matchChars(string.toList)

  def token(string: String): P[Unit] =
    matchString(string) *> whitespace

  val identifier: P[String] = {
    lazy val iPart: P[Char => List[Char]] =
      (readIf(Character.isJavaIdentifierPart(_: Char)) <*> iPart)
        .map((t: List[Char]) => (_: Char) :: t) <+>
        whitespace.map(_ => (_: Char) :: Nil)
    (readIf(Character.isJavaIdentifierStart(_: Char)) <*> iPart)
      .map(_.mkString) <* whitespace
  }

  def separated[A](p: P[A])(separator: P[Unit]): P[List[A]] = {
    lazy val tail: P[List[A]] =
      (p <*> separator *> tail.map((t: List[A]) => (_: A) :: t)) <+>
        List.empty[A].point[P]
    tail
  }

  def parenthetical[A](p: P[A]) = token('(') *> p <* token(')') //

  def tupled[A](p: P[A]): P[List[A]] =
    token('<') *>
      separated(p <+> error("improper tuple member"))(token(',')) <*
      token('>')

  val arrow = token("->")

  val fix = token('@')

  val typeExp: P[T] = {
    lazy val arrowTail: P[T => T] =
      (arrow *> bound.map((t: T) => T.arrow(_, t))) <+>
        ((t: T) => t).point[P]

    lazy val bound: P[T] =
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

  val integer: P[Int] = {
    lazy val digits: P[Int => Int] =
      (readIf(Character.isDigit(_: Char))
        .map((c: Char) => (acc: Int) => 10 * acc + c - '0') <*> digits
        .map((f: Int => Int) => f.compose(_: Int => Int))) <+>
        ((i: Int) => i).point[P]
    digits.map(_(0))
  }

  val defExp: P[D] = {

    lazy val unit: P[D] =
      parenthetical(intros) <+>
        tupled(intros).map(D.tuple(_)) <+>
        identifier.map(D.variable(_)) <+>
        error("not a delimited def")

    val elim: P[D => D] =
      (readIf(_ == '_') *> integer.map((i: Int) => D.project(_, i))) <+>
        token('\'').map(_ => D.unfold(_)) <+>
        unit.map((operand: D) => D.application(_, operand)) <+>
        ((d: D) => d).point[P]
    lazy val elims: P[D => D] = elim <*> elims
      .map((f: D => D) => f.compose(_: D => D)) <+>
      ((d: D) => d).point[P]

    lazy val intros: P[D] =
      (identifier <*> {
        (token('=') *> (intros <*> token(';') *> intros.map(
          (c: D) => (b: D) => D.let(_, b, c)
        ))) <+>
          (arrow *> intros.map((b: D) => D.abstraction(_, b))) <+>
          (fix *> intros.map((b: D) => D.fix(_, b)))
      }) <+>
        (unit <*> elims)

    intros
  }
}
