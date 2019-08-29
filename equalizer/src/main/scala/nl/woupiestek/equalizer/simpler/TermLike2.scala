package nl.woupiestek.equalizer.simpler

trait TermLike2[I, T, U] {

  def variable(id: I): U

  def lambda(id: I, body: T): U

  def apply(operator: T, operand: T): U

  def let(id: I, value: T, context: T): U

  def check(left: T, right: T, context: T): U

}
