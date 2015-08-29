/**
 * Created by Wouter on 28-8-2015.
 */

package nl.woupiestek.andrej.parse

import org.parboiled2._
import nl.woupiestek.andrej.core._

sealed trait TokenTree

case class Leaf(content: String) extends TokenTree

case class Fork(subtrees: List[TokenTree]) extends TokenTree

class Lexer(val input: ParserInput) extends Parser {
  def full: Rule1[TokenTree] = rule {
    expression ~ EOI
  }

  def expression: Rule1[TokenTree] = rule {
    paren | token
  }

  def token: Rule1[Leaf] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum)) ~> Leaf ~ space
  }

  def paren: Rule1[Fork] = rule {
    leftDelimiter ~ zeroOrMore(expression).separatedBy(separator) ~> fork ~ rightDelimiter
  }

  def leftDelimiter:Rule0 = rule {
    '(' ~ space
  }

  def rightDelimiter:Rule0 = rule {
    ')' ~ space
  }

  def separator:Rule0 = rule {
    (spaceChar | ',') ~ space
  }

  def space = rule {
    zeroOrMore(spaceChar)
  }

  def spaceChar = rule {
    anyOf(" \n\r\t\f")
  }

  def fork = (r: Seq[TokenTree]) => Fork(r toList): Fork //works only with very explicit typing.
}

object Lex {
  def apply(input: ParserInput) = new Lexer(input).full.run()
}

object Parse2 {

  //ik weet ongeveer wat het algoritme en het eindresultaat moeten zijn.
  //curryen nemen we aan
  //maar hoe nu verder?

  val LAMBDA = "function"
  val UNIVERSE = "type"
  val PI = "product"
  val CUT = "let"

  def apply(tokenTree: TokenTree): Option[Expression] = tokenTree match {
    case Leaf(name) => Some(Named(name))
    case Fork(Leaf(LAMBDA) :: tail) => abstraction(tail)
    case Fork(Leaf(UNIVERSE) :: tail) => universe(tail)
    case Fork(Leaf(PI) :: tail) => product(tail)
    case Fork(Leaf(CUT) :: tail) => cut(tail)
    case Fork(other) => application(other)
  }

  def application(tokenTree: List[TokenTree]): Option[Expression] = tokenTree.foldLeft[Option[Expression]](None) {
    case (None, tree) => apply(tree)
    case (Some(tree), tree2) => apply(tree2).map {
      Application(tree, _)
    }
  }

  def abstraction(list: List[TokenTree]): Option[Expression] = list match {
    case Leaf(name) :: next :: tail => for {
      t <- apply(next)
      a <- application(tail)
    } yield Abstraction(Named(name), t, a)
    case _ => None
  }

  def product(list: List[TokenTree]): Option[Expression] = list match {
    case Leaf(name) :: next :: tail => for {
      t <- apply(next)
      a <- application(tail)
    } yield Pi(Abstraction(Named(name), t, a))
    case _ => None
  }

  def universe(list: List[TokenTree]): Option[Expression] = list match {
    case Leaf(level) :: Nil => Some(Universe(level.toInt))
    case _ => None
  }

  def cut(list: List[TokenTree]): Option[Expression] = list match {
    case Leaf(name) :: next :: tail => for {
      t <- apply(next)
      a <- application(tail)
    } yield Cut(Named(name), t, a)
    case _ => None
  }
}