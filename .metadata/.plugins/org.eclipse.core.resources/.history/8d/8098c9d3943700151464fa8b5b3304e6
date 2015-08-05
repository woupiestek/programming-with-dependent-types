package nl.woupiestek.andrej

/**
 * @author Wouter
 *
 */
trait Expression
trait Terminal extends Expression
trait Variable extends Terminal
case class Application(operator: Expression, operand: Expression) extends Expression
case class Universe(level: Int) extends Terminal
case class Abstraction(variable: Variable, domain: Expression, term: Expression) extends Expression
case class Product(typeMap: Abstraction) extends Expression
case class Cut(variable: Variable, left: Expression, right: Expression) extends Expression
case class Named(name: String) extends Variable
