package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import scalaz._
import scalaz.Scalaz._

class Grammar[P[_]: ApplicativePlus](
    readIf: (Char => Boolean) => P[Char]
) {

  def term[T](T: TermLike[String, T]): P[T] =
    (name |@| list(mod(T)))(
      (x, y) => y.foldLeft(T.variable(x))((a, b) => b(a))
    )

  private def mod[T](T: TermLike[String, T]): P[T => T] =
    ((keyword("be") *> name) |@| term(T))(
      (x, y) => T.let(x, y, _)
    ) <+>
      (keyword("for") *> name).map(x => T.lambda(x, _)) <+>
      ((keyword("if") *> term(T)) |@| (keyword("is") *> term(
        T
      )))(
        (x, y) => T.check(x, y, _)
      ) <+>
      term(T).map(x => T.operate(x, _))

  val space: P[Unit] = readIf(isWhitespace) *> space <+> ()
    .point[P]

  def gather[X](rule: P[X]): P[List[X]] =
    (rule |@| gather(rule))(_ :: _) <+> List.empty[X].point[P]

  def readWhile(f: Char => Boolean): P[List[Char]] =
    gather(readIf(f))

  val name: P[String] =
    (readIf(isUpperCase) |@| readWhile(isLetterOrDigit) <* space) {
      (h, t) =>
        (h :: t).toList.mkString
    }

  def symbol(c: Char): P[Char] = is(c) <* space

  private def is(c: Char): P[Char] = readIf((_: Char) == c)

  def keyword(s: String): P[Unit] =
    s.foldLeft(().pure[P])(_ <* is(_)) <* space

  def list[O](rule: P[O]): P[List[O]] =
    nel(rule) <+> List.empty[O].point[P]

  def nel[O](rule: P[O]): P[List[O]] = {
    implicit val inst: Monoid[P[List[O]]] =
      PlusEmpty[P].monoid[List[O]]
    (rule |@| List(',', '.', ';', '&').foldMap(
      b => gather((symbol(b) *> rule))
    ))(
      _ :: _
    )
  }
}
