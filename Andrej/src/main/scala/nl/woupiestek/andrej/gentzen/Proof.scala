package nl.woupiestek.andrej.gentzen

sealed trait Proof {
  def applied(values: List[Proof]): Proof

  def reduce(context: Context): Proof
}

sealed trait Context {
  def get(index: Int): Proof
}

object Context {

  case object Empty extends Context {
    override def get(index: Int): Proof = Antecedent(index)
  }

  case class Pushed(head: List[Proof], tail: Context = Empty) extends Context {
    override def get(index: Int): Proof = head lift index getOrElse (tail get (index - head.length))
  }

  case class Shifted(amount: Int, tail: Context = Empty) extends Context {
    override def get(index: Int): Proof =
      if (index < amount) Antecedent(index)
      else tail get (index - amount)
  }

  case class Send(index: Int, arguments: List[Proof], base: Context = Empty) extends Context {
    override def get(j: Int): Proof =
      if (j == index) base.get(j).applied(arguments)
      else base.get(j)
  }

}

//axiom
case class Antecedent(index: Int) extends Proof {
  override def applied(values: List[Proof]): Proof = ArrowLeft(index, values, this)

  override def reduce(context: Context): Proof = context get index
}

//occasionally needed for contraction: copy an element
case class Copy(index: Int, continuation: Proof) extends Proof {
  override def applied(values: List[Proof]): Proof = Copy(index, continuation.applied(values))

  override def reduce(context: Context): Proof = Push(List(Antecedent(index)), continuation).reduce(context)
}

//arrow introductions
case class ArrowRight(arity: Int, continuation: Proof) extends Proof {
  override def applied(values: List[Proof]): Proof =
    if (values.length >= arity) {
      val (head, tail) = values.splitAt(arity)
      continuation.reduce(Context.Pushed(head)).applied(tail)
    } else ArrowRight(arity - values.length, continuation.reduce(Context.Pushed(values)))

  override def reduce(context: Context): Proof =
    ArrowRight(arity, continuation.reduce(Context.Shifted(arity, context)))
}

case class ArrowLeft(index: Int, arguments: List[Proof], continuation: Proof) extends Proof {
  override def applied(values: List[Proof]): Proof = ArrowLeft(index, arguments, continuation.applied(values))

  override def reduce(context: Context): Proof = {
    val cutFrees = arguments.map(_.reduce(context))
    continuation.reduce(Context.Send(index, cutFrees, context))
  }
}

//cut rule
case class Push(values: List[Proof], continuation: Proof) extends Proof {
  override def applied(values2: List[Proof]): Proof = Push(values, continuation.applied(values2))

  override def reduce(context: Context): Proof = {
    val cutFrees = values.map(_.reduce(context))
    continuation.reduce(Context.Pushed(cutFrees, context))
  }
}

