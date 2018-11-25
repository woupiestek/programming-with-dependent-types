package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import nl.woupiestek.equalizer.parsing.Rule
import nl.woupiestek.equalizer.parsing.Rule._
import nl.woupiestek.equalizer.simpler.Grammar._
import shapeless._

import scala.language.postfixOps

class Grammar[T](implicit T: TermLike[String, T]) {

  lazy val term: Rule[Char, T] =
    (name ~ join(
      nil,
      mod ::: join(
        nil,
        punct.flatMap(c => mod ::: (symbol(c) > mod).zeroOrMore))).$).map {
      case x :: y :: HNil => y.foldLeft(T.variable(x))((a, b) => b(a))
    }

  private lazy val mod: Rule[Char, T => T] = join(
    ((keyword("be") > name) ~ term.$).map {
      case x :: y :: HNil => T.let(x, y, _)
    },
    (keyword("for") > name).map(x => T.lambda(x, _)),
    ((keyword("if") > term) ~ (keyword("is") > term).$).map {
      case x :: y :: HNil => T.check(x, y, _)
    },
    term.map(x => T.apply(_, x)))
}

object Grammar {
  val space: Rule[Char, Unit] = readIf(isWhitespace).zeroOrMore.ignore

  def spaced[X](rule: Rule[Char, X]): Rule[Char, X] =
    rule < space

  val name: Rule[Char, String] =
    spaced(readIf(isUpperCase) ::: readIf(isLetterOrDigit).zeroOrMore)
      .map(_.mkString)

  def symbol(c: Char): Rule[Char, Unit] = spaced(readIf[Char](_ == c)).ignore

  def keyword(name: String): Rule[Char, Unit] =
    spaced(sequence(name.map(c => readIf[Char](_ == c)).toList)).ignore

  val punct: Rule[Char, Char] = spaced(readIf(Set(',', '.', ';', '&')))

  def count(from: Int): Rule[Char, Int] =
    join(read[Char] > count(from + 1), unit[Char, Int](from))

}
