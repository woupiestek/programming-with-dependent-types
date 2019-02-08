package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import nl.woupiestek.equalizer.parsing.ParserT
import nl.woupiestek.equalizer.parsing.ParserT._
import scalaz._
import scalaz.Scalaz._

class Grammar[T](implicit T: TermLike[String, T]) {
  type P[A] = ParserT[Char, A]

  lazy val term: P[T] =
    (name |@| list(mod))((x, y) => y.foldLeft(T.variable(x))((a, b) => b(a)))

  private lazy val mod: P[T => T] =
    ((keyword("be") *> name) |@| term)((x, y) => T.let(x, y, _)) <+>
      (keyword("for") *> name).map(x => T.lambda(x, _)) <+>
      ((keyword("if") *> term) |@| (keyword("is") *> term))(
        (x, y) => T.check(x, y, _)
      ) <+>
      term.map(x => T.operate(x, _))

  val space: P[Unit] = If(isWhitespace).scanMap(_ => ())

  val name: P[String] =
    (If(isUpperCase).one |@| If(isLetterOrDigit).one.list <* space) { (h, t) =>
      (h :: t).toList.mkString
    }

  def symbol(c: Char): P[Char] = is(c) <* space

  private def is(c: Char): P[Char] = If((_: Char) == c).one

  def keyword(s: String): P[Unit] =
    s.foldLeft(().pure[P])(_ <* is(_)) <* space

  def list[O](rule: P[O]): P[IList[O]] =
    nel(rule).map(_.list) <+> IList.empty[O].point[P]

  def nel[O](rule: P[O]): P[NonEmptyList[O]] = {
    implicit val inst: Monoid[P[IList[O]]] = PlusEmpty[P].monoid[IList[O]]
    (rule |@| List(',', '.', ';', '&').foldMap(b => (symbol(b) *> rule).list))(
      NonEmptyList.nel[O]
    )
  }
}
