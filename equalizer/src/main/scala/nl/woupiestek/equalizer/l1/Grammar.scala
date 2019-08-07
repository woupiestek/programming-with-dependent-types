package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.Grammatical
import Grammar._

abstract class Grammar[P[- _, + _]: Grammatical, T, D](
    T: AST.Type[T],
    D: AST.Def[D]
) {
  private type Q[A] = P[Input[P], A]
  private implicit val monadPlus: ApplicativePlus[Q] =
    Grammatical[P].monadPlus[Input[P]]

  private def readIf(f: Char => Boolean): Q[Char] =
    Grammatical[P].ask((_: Input[P]).readIf(f))
  private def error[A](message: String): Q[A] =
    Grammatical[P].ask((_: Input[P]).error(message))

  lazy val whitespace: P[Input[P], Unit] =
    (readIf(Character.isWhitespace(_: Char)) *> whitespace) <+> ().point[Q]

  def token(char: Char): P[Input[P], Unit] = readIf(_ == char) *> whitespace

  def matchChars(chars: List[Char]): P[Input[P], Unit] = chars match {
    case Nil    => ().point[Q]
    case h :: t => readIf(_ == h) *> matchChars(t)
  }

  def matchString(string: String) = matchChars(string.toList)

  def token(string: String): P[Input[P], Unit] =
    matchString(string) *> whitespace

  val identifier: P[Input[P], String] = {
    lazy val iPart: P[Input[P], Char => List[Char]] =
      (readIf(Character.isJavaIdentifierPart(_: Char)) <*> iPart)
        .map((t: List[Char]) => (_: Char) :: t) <+>
        whitespace.map(_ => (_: Char) :: Nil)
    (readIf(Character.isJavaIdentifierStart(_: Char)) <*> iPart)
      .map(_.mkString) <* whitespace
  }

  def separated[A](
      p: P[Input[P], A]
  )(separator: P[Input[P], Unit]): P[Input[P], List[A]] = {
    lazy val tail: P[Input[P], List[A]] =
      (p <*> separator *> tail.map((t: List[A]) => (_: A) :: t)) <+>
        List.empty[A].point[Q]
    tail
  }

  def parenthetical[A](p: P[Input[P], A]) = token('(') *> p <* token(')') //

  def tupled[A](p: P[Input[P], A]): P[Input[P], List[A]] =
    token('<') *>
      separated(p <+> error("improper tuple member"))(token(',')) <*
      token('>')

  val arrow = token("->")

  val fix = token('@')

  val typeExp: P[Input[P], T] = {
    lazy val arrowTail: P[Input[P], T => T] =
      (arrow *> bound.map((t: T) => T.arrow(_, t))) <+>
        ((t: T) => t).point[Q]

    lazy val bound: P[Input[P], T] =
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

  val integer: P[Input[P], Int] = {
    lazy val digits: P[Input[P], Int => Int] =
      (readIf(Character.isDigit(_: Char))
        .map((c: Char) => (acc: Int) => 10 * acc + c - '0') <*> digits
        .map((f: Int => Int) => f.compose(_: Int => Int))) <+>
        ((i: Int) => i).point[Q]
    digits.map(_(0))
  }

  val defExp: P[Input[P], D] = {

    def unit: P[Input[P], D] =
      parenthetical(intros) <+>
        tupled(intros).map(D.tuple(_)) <+>
        identifier.map(D.variable(_)) <+>
        error("not a delimited def")

    def elim: P[Input[P], D => D] =
      (readIf(_ == '_') *> integer.map((i: Int) => D.project(_, i))) <+>
        token('\'').map(_ => D.unfold(_)) <+>
        unit.map((operand: D) => D.application(_, operand)) <+>
        ((d: D) => d).point[Q]
    def elims: P[Input[P], D => D] =
      elim <*> elims
        .map((f: D => D) => f.compose(_: D => D)) <+>
        ((d: D) => d).point[Q]

    def intros: P[Input[P], D] =
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

object Grammar {
  trait Input[P[- _, + _]] {
    def readIf(f: Char => Boolean): P[Input[P], Char]
    def error(message: String): P[Input[P], Nothing]
  }
}
