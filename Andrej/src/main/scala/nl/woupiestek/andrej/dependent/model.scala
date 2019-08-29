package nl.woupiestek.andrej.dependent

sealed trait Expression {
  val ind: Option[Int]

  val subs: List[Expression]
}

case class Get(index: Int) extends Expression {
  override val subs: List[Expression] = Nil

  override val ind: Option[Int] = Some(index)
}

case class Push(value: Expression, continuation: Expression)
    extends Expression {
  override val subs: List[Expression] =
    List(value, continuation)

  override val ind: Option[Int] = None
}

case class Function(dom: Expression, continuation: Expression)
    extends Expression {
  override val subs: List[Expression] = List(dom, continuation)

  override val ind: Option[Int] = None
}

case class Apply(
    index: Int,
    argument: Expression,
    continuation: Expression
) extends Expression {
  override val subs: List[Expression] =
    List(argument, continuation)

  override val ind: Option[Int] = Some(index)
}

case class Product(dom: Expression, continuation: Expression)
    extends Expression {
  override val ind: Option[Int] = None

  override val subs: List[Expression] = List(dom, continuation)
}

case object Omega extends Expression {
  override val ind: Option[Int] = None

  override val subs: List[Expression] = Nil
}

//final
