package nl.woupiestek.equalizer.simpler

import java.lang.Character._

import nl.woupiestek.equalizer.parsing.Rule
import nl.woupiestek.equalizer.parsing.Rule._
import nl.woupiestek.equalizer.simpler.Grammar._
import scalaz.Scalaz._

import scala.language.{postfixOps, reflectiveCalls}

class Grammar[T](implicit T: TermLike[String, T]) {

  lazy val term: R[T] =
    (name |@| list(mod)) ((x, y) => y.foldLeft(T.variable(x))((a, b) => b(a)))

  private lazy val mod: R[T => T] = or(
    ((keyword("be") > name) |@| term) ((x, y) => T.let(x, y, _)),
    (keyword("for") > name).map(x => T.lambda(x, _)),
    ((keyword("if") > term) |@| (keyword("is") > term)) ((x, y) => T.check(x, y, _)),
    term.map(x => T.apply(_, x)))
}

object Grammar {
  type R[X] = Rule[Char, X]

  val space: R[List[Char]] = readIf(isWhitespace).zeroOrMore

  val name: R[String] =
    ((readIf(isUpperCase) ::: readIf(isLetterOrDigit).zeroOrMore) < space)
      .map(_.mkString)

  def symbol(c: Char): R[List[Char]] = (is(c) |@| space) (_ :: _)

  private def is(c: Char): R[Char] = readIf[Char](_ == c)

  def keyword(s: String): R[List[Char]] = (s.toList.traverse(is) |@| space) (_ ++ _)

  private val punct: R[Char] = readIf(Set(',', '.', ';', '&')) < space

  def list[O](rule: R[O]): R[List[O]] = or(nel(rule), unit(Nil))

  def nel[O](rule: R[O]): R[List[O]] =
    rule ::: or(
      punct.flatMap(c => rule ::: (symbol(c) > rule).zeroOrMore),
      unit(Nil))
}