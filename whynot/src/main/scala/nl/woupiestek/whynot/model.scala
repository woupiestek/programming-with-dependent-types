package nl.woupiestek.whynot

/**
  * Created by Wouter on 9-4-2016.
  */
class model {}

sealed trait Value[E]

case class Identifier[E](index: Int) extends Value[E]

case class Application[E](operator: E, operand: E)
    extends Value[E]

case class Abstraction[E](domain: E, body: E) extends Value[E]

case class Universe[E](index: Int) extends Value[E]

case class Product[E](domain: E, body: E) extends Value[E]

case class Fail[E]() extends Value[E]

trait Computation {
  def resume: Value[Computation]

  def sort: Computation
}
