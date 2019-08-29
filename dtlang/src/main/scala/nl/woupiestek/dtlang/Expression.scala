package nl.woupiestek.dtlang

sealed trait Expression

object Expression {

  case class Variable(index: Int) extends Expression

  case class Application(
      operator: Expression,
      operands: Seq[Expression]
  ) extends Expression

  case class Abstraction(
      types: Seq[Expression],
      continuation: Expression
  ) extends Expression

  case object Universe extends Expression

  case class Universe(level: Int) extends Expression

  case class Product(typeMap: Expression) extends Expression

}
