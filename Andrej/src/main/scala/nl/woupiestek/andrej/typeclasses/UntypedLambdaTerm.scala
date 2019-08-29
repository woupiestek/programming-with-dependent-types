package nl.woupiestek.andrej.typeclasses

trait UntypedLambdaTerm[L] {
  def variable(index: Int): L

  def application(operator: L, operand: L): L

  def abstraction(term: L): L
}

object UntypedLambdaTerm {

  def $[L](index: Int)(implicit L: UntypedLambdaTerm[L]): L =
    L.variable(index)

  def \[L](term: L)(implicit L: UntypedLambdaTerm[L]): L =
    L.abstraction(term)

  implicit class LOps[L](operator: L) {
    def *(operand: L)(implicit L: UntypedLambdaTerm[L]): L =
      L.application(operator, operand)
  }

}
