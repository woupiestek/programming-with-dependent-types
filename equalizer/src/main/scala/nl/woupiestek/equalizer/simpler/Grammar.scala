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

  val name: Rule[Char, String] =
    (readIf(isUpperCase) ::: readIf(isLetterOrDigit).zeroOrMore)
      .map(_.mkString) < space

  def symbol(c: Char): Rule[Char, Unit] = readIf[Char](_ == c).ignore < space

  def keyword(name: String): Rule[Char, Unit] =
    traverse(name.toList)(c => readIf[Char](_ == c)).ignore < space

  val punct: Rule[Char, Char] = readIf(Set(',', '.', ';', '&')) < space

  def count[I](from: Int): Rule[I, Int] =
    join(read > count(from + 1), unit(from))

}
