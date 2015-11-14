package nl.woupiestek.andrej

/**
  * Created by Wouter on 9-11-2015.
  */
trait Lambda {

  type Identifier

  sealed trait Term

  case class Variable(name: Identifier) extends Term

  case class Application(operator: Term, operands: List[Term]) extends Term

  case class Abstraction(pairs: List[(Identifier, Term)], body: Term) extends Term

  case class Product(pairs: List[(Identifier, Term)], body: Term) extends Term

  case class Type(level:Int) extends Term

  case class Assignment(key:Identifier,value:Term)

  case class Sequence(assignments: List[Assignment], conclusion: Term) extends Term

}




