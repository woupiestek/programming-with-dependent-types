package nl.woupiestek.andrej

sealed trait Expression

sealed trait Statement

object Lambda {
  type Identifier = String

  case class Typing(identifier: Identifier, typeValue: Expression) extends Statement

  case class Assignment(identifier: Identifier, value: Expression) extends Statement

  case class Variable(identifier: Identifier) extends Expression

  case class Type(level: Int) extends Expression

}

case class Application(operator: Expression, operands: List[Expression]) extends Expression

case class Abstraction(arguments: List[Statement], body: Expression) extends Expression

case class Product(typeMap: Expression) extends Expression
