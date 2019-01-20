package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import nl.woupiestek.equalizer.parsing.Rule
import nl.woupiestek.equalizer.parsing.Rule._
import scalaz.Scalaz._

import scala.language.{higherKinds, postfixOps, reflectiveCalls}

class Grammar[T, R[_]](implicit T: TermLike[String, T], R: Rule[R, Char]) {

  lazy val term: R[T] =
    (name |@| list(mod)) ((x, y) => y.foldLeft(T.variable(x))((a, b) => b(a)))

  private lazy val mod: R[T => T] =
    ((keyword("be") > name) |@| term) ((x, y) => T.let(x, y, _)) <+>
      (keyword("for") > name).map(x => T.lambda(x, _)) <+>
      ((keyword("if") > term) |@| (keyword("is") > term)) ((x, y) => T.check(x, y, _)) <+>
      term.map(x => T.apply(_, x))

  val space: R[List[Char]] = R.readIf(isWhitespace).zeroOrMore

  val name: R[String] =
    ((R.readIf(isUpperCase) |::| R.readIf(isLetterOrDigit).zeroOrMore) < space)
      .map(_.mkString)

  def symbol(c: Char): R[List[Char]] = is(c) |::| space

  private def is(c: Char): R[Char] = R.readIf(_ == c)

  def keyword(s: String): R[List[Char]] =
    s.toList.traverse[R, Char](is) < space

  def list[O](rule: R[O]): R[List[O]] =
    nel(rule) <+> List.empty[O].point[R]

  def nel[O](rule: R[O]): R[List[O]] = rule |::|
    Set(',', '.', ';', '&').foldLeft(R.empty[List[O]])((a, b) =>
      a <+> (symbol(b) > rule).zeroOrMore)
}