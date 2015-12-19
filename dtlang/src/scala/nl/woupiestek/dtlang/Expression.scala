package nl.woupiestek.dtlang

sealed trait Expression

object Expression {

  case class Variable(index: Int) extends Expression

  case class Application(operator: Expression, operands: Seq[Expression]) extends Expression

  case class Abstraction(types: List[Expression], continuation: Expression) extends Expression

  case object Universe extends Expression

  case class Product(typeMap: Expression) extends Expression

}