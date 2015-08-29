package nl.woupiestek.andrej.parse

import util.parsing.combinator._
import nl.woupiestek.andrej.core._
import scala.util.matching.Regex

/**
 * @author Wouter
 */
object Parse extends RegexParsers {
  
  def expression: Parser[Expression] = simple | application | abstraction | pi | cut
  
  def variable: Parser[Variable] = """[a-zA-Z]\w*""".r ^^ { (x:String) => Named(x) }

  def universe: Parser[Universe] = {
    val typed = """Type (\d+)""".r
    typed ^^ { case typed(n) => Universe(n.toInt) }
  }
 
  def application: Parser[Application] = simple ~ simple ^^ { case x ~ y => Application(x,y) }
  
  def simple: Parser[Expression] = universe | variable | paren
  
  def paren: Parser[Expression] = "(" ~ expression ~ ")" ^^ { case "(" ~ e ~ ")" => e }

  def abstraction: Parser[Abstraction] = "fun " ~ variable ~ ":" ~ expression ~ "=>" ~ expression ^^ { case "fun " ~ v ~ ":"~ e0 ~"=>" ~ e1 => Abstraction(v,e0,e1) }

  def pi: Parser[Pi] = "forall " ~ variable ~ ":" ~ expression ~ "=>" ~ expression ^^ { case "forall " ~ v ~ ":" ~ e0 ~ "=>" ~ e1 => Pi(Abstraction(v,e0,e1)) }
  
  def cut: Parser[Cut] = "[" ~ variable ~ "=" ~ expression ~ "]" ~ expression ^^ { case "[" ~ v ~ "=" ~ e0 ~ "]" ~ e1 => Cut(v,e0,e1) }

  def apply(input: String) = parseAll(expression, input)
}