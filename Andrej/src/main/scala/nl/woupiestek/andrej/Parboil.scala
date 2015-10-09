package nl.woupiestek.andrej

import org.parboiled2._


class Parboil(val input: ParserInput) extends Parser {
  type E = Expr[String]

  def full = rule(zeroOrMore(white) ~ expression ~ EOI)

  def expression: Rule1[E] = rule(abstraction | product | application | universe | variable)

  def variable: Rule1[Iden[String]] = rule {
    capture(oneOrMore(CharPredicate.Alpha)) ~> ((x: String) => Iden(x)) ~ zeroOrMore(white)
  }

  def universe: Rule1[E] = rule("Type" ~ zeroOrMore(white) ~ push(Type))

  def abstraction: Rule1[E] = rule(spaced('\\') ~ body)

  def abst = (x: Iden[String], y: E, z: E) => Abst(y, z abstracted (x name))

  def body: Rule1[Abst[String]] = rule(variable ~ spaced(':') ~ expression ~ spaced('.') ~ expression ~> abst)

  def application: Rule1[E] = rule {
    spaced('(') ~ oneOrMore(expression) ~> {
      (es: Seq[Expr[String]]) => Appl(es head, (es tail) toList)
    } ~ spaced(')')
  }

  def product: Rule1[E] = rule("Prod" ~ white ~ body ~> ((a: Abst[String]) => Prod(a)))

  def white: Rule0 = rule(anyOf(" \n\r\t\f"))

  def spaced(x: Char): Rule0 = rule(x ~ zeroOrMore(white))

  //def spaced(x: =>Rule0): Rule0 = rule(x ~ zeroOrMore(white))

}


object Parboil {
  def apply(input: ParserInput) = new Parboil(input).full.run()
}