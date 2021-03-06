package nl.woupiestek.equalizer.simpler

sealed abstract class Term[I, T]

object Term {

  case class Variable[I, T](id: I) extends Term[I, T]

  case class Lambda[I, T](id: I, body: T) extends Term[I, T]

  case class Apply[I, T](operator: T, operands: T)
      extends Term[I, T]

  case class Let[I, T](id: I, value: T, context: T)
      extends Term[I, T]

  case class Check[I, T](left: T, right: T, context: T)
      extends Term[I, T]

  class Instance[I] extends TermLike[I, List[Term[I, Int]]] {
    override def variable(id: I): List[Term[I, Int]] =
      Variable[I, Int](id) :: Nil

    override def lambda(
        id: I,
        body: List[Term[I, Int]]
    ): List[Term[I, Int]] =
      Lambda[I, Int](id, body.length) :: body

    override def operate(
        operator: List[Term[I, Int]],
        operand: List[Term[I, Int]]
    ): List[Term[I, Int]] =
      Apply[I, Int](
        operator.length + operand.length,
        operand.length
      ) :: operator ++ operand

    override def let(
        id: I,
        value: List[Term[I, Int]],
        context: List[Term[I, Int]]
    ): List[Term[I, Int]] =
      Let[I, Int](
        id,
        value.length + context.length,
        context.length
      ) :: value ++ context

    override def check(
        left: List[Term[I, Int]],
        right: List[Term[I, Int]],
        context: List[Term[I, Int]]
    ): List[Term[I, Int]] =
      Check[I, Int](
        left.length + right.length + context.length,
        right.length + context.length,
        context.length
      ) :: left ++ right ++ context
  }

}
