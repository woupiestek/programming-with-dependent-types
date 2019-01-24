package nl.woupiestek.equalizer.simpler

trait TermLike[I, T] {

  def variable(id: I): T

  def lambda(id: I, body: T): T

  def operate(operator: T, operand: T): T

  def let(id: I, value: T, context: T): T

  def check(left: T, right: T, context: T): T

}

object TermLike {

  def freeVariables[I]: TermLike[I, Set[I]] = new TermLike[I, Set[I]] {
    override def variable(id: I): Set[I] = Set(id)

    override def lambda(id: I, body: Set[I]): Set[I] = body - id

    override def operate(operator: Set[I], operand: Set[I]): Set[I] =
      operator ++ operand

    override def let(id: I, value: Set[I], context: Set[I]): Set[I] =
      context - id ++ value

    override def check(left: Set[I], right: Set[I], context: Set[I]): Set[I] =
      left ++ right ++ context
  }

  def size[I]: TermLike[I, Int] = new TermLike[I, Int] {
    override def variable(id: I): Int = 1

    override def lambda(id: I, body: Int): Int = 2 + body

    override def operate(operator: Int, operand: Int): Int = 1 + operand + operator

    override def let(id: I, value: Int, context: Int): Int = 2 + value + context

    override def check(left: Int, right: Int, context: Int): Int = 1 + left + right + context
  }

}