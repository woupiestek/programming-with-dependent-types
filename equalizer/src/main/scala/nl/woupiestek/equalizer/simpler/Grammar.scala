package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import nl.woupiestek.equalizer.parsing.Rule
import nl.woupiestek.equalizer.parsing.Rule._
import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds

class Grammar[T, R[_]](implicit T: TermLike[String, T], R: Rule[R, Char]) {

  lazy val term: R[T] =
    (name |@| list(mod)) ((x, y) => y.foldLeft(T.variable(x))((a, b) => b(a)))

  private lazy val mod: R[T => T] =
    ((keyword("be") *> name) |@| term) ((x, y) => T.let(x, y, _)) <+>
      (keyword("for") *> name).map(x => T.lambda(x, _)) <+>
      ((keyword("if") *> term) |@| (keyword("is") *> term)) ((x, y) => T.check(x, y, _)) <+>
      term.map(x => T.operate(x, _))

  val space: R[Unit] = readWhile[R, Char, Unit](isWhitespace)(_ => ())

  val name: R[String] =
    (readIf(isUpperCase) |@| readIf(isLetterOrDigit).list <* space) {
      (h, t) => (h :: t).foldLeft(new StringBuilder)(_ += _).toString
    }

  def symbol(c: Char): R[Char] = is(c) <* space

  private def is(c: Char): R[Char] = readIf(_ == c)

  def keyword(s: String): R[Unit] =
    s.foldLeft(().pure[R])(_ <* is(_)) <* space

  def list[O](rule: R[O]): R[IList[O]] =
    nel(rule).map(_.list) <+> IList.empty[O].point[R]

  def nel[O](rule: R[O]): R[NonEmptyList[O]] = {
    implicit val inst: Monoid[R[IList[O]]] = PlusEmpty[R].monoid[IList[O]]
    (rule |@| List(',', '.', ';', '&').foldMap(b => (symbol(b) *> rule).list)) (
      NonEmptyList.nel[O])
  }
}