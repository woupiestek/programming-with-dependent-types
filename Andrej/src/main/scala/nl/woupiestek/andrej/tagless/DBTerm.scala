package nl.woupiestek.andrej.tagless

trait DBTerm[T] {

  def variable(index: Int): T

  def universe(index: Int): T

  def product(typ: T, body: T): T

  def abstraction(typ: T, body: T): T

  def application(operator: T, operand: T): T

}