package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import nl.woupiestek.equalizer.parsing.Rule
import nl.woupiestek.equalizer.parsing.Rule._
import nl.woupiestek.equalizer.simpler.Grammar._
import scalaz.Scalaz._

import scala.language.{postfixOps, reflectiveCalls}

class Grammar[T](implicit T: TermLike[String, T]) {

  lazy val term: Rule[Char, T] =
    (name |@| list(mod)) ((x, y) => y.foldLeft(T.variable(x))((a, b) => b(a)))

  private lazy val mod: Rule[Char, T => T] = or(
    ((keyword("be") > name) |@| term) ((x, y) => T.let(x, y, _)),
    (keyword("for") > name).map(x => T.lambda(x, _)),
    ((keyword("if") > term) |@| (keyword("is") > term)) ((x, y) => T.check(x, y, _)),
    term.map(x => T.apply(_, x)))
}

object Grammar {
  val space: Rule[Char, List[Char]] = readIf(isWhitespace).zeroOrMore

  val name: Rule[Char, String] =
    ((readIf(isUpperCase) ::: readIf(isLetterOrDigit).zeroOrMore) < space)
      .map(_.mkString)

  def symbol(c: Char): Rule[Char, Unit] =
    (readIf[Char](_ == c) |@| space)((_,_) => ())

  def keyword(s: String): Rule[Char, Unit] =
    (traverse(s.toList)(c => readIf[Char](_ == c)) |@| space)((_,_) => ())

  val punct: Rule[Char, Char] = readIf(Set(',', '.', ';', '&')) < space

  def list[O](rule: Rule[Char, O]): Rule[Char, List[O]] = or(nel(rule), nil)

  def nel[O](rule: Rule[Char, O]): Rule[Char, List[O]] =
    rule ::: or(
      punct.flatMap(c => rule ::: (symbol(c) > rule).zeroOrMore),
      nil)
}