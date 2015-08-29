package nl.woupiestek.andrej.parse

import org.parboiled2._
import nl.woupiestek.andrej.core._

class Parboil(val input: ParserInput) extends Parser {

  def full = rule { expression ~ EOI }

  def expression: Rule1[Expression] = rule { abstraction | product | cut | application | universe | variable }
  
  def variable = rule { capture(oneOrMore(CharPredicate.Alpha)) ~> Named ~ whiteSpace }
  def universe = rule { 
    "type " ~ capture(oneOrMore(CharPredicate.Digit)) ~> { (s:String) => Universe(s.toInt) } ~ whiteSpace 
  }
  def abstraction = rule { 
    "(fun " ~ variable ~ ':' ~ expression ~ "=>" ~ expression ~ ')' ~> Abstraction
  }
  def application = rule { '(' ~ expression ~ expression ~ ')' ~> Application }
  def product = rule { 
    "(forall " ~ variable ~ ':' ~ expression ~ "=>" ~ expression ~ ')' ~> {
      (x:Variable,y:Expression,z:Expression) => Pi(Abstraction(x,y,z))
    }
  }
  def cut = rule { "(let "  ~ variable ~ '=' ~ expression ~ '.' ~ expression ~ ')' ~> Cut }

  def whiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  
}


object Parboil {
  def apply(input: ParserInput) = new Parboil(input).full.run()
}