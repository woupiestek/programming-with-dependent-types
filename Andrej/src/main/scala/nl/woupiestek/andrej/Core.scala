package nl.woupiestek.andrej

// terms should form a sort of free monad
// hence the genericity and flatMap operators down here
trait Expr[+I] {
  def flatMap[J](f: I => Expr[J]): Expr[J]

  def @:[J >: I](operator: Expr[Option[J]]): Expr[J] = operator.flatMap {
    case None => this
    case Some(iden) => Iden(iden)
  }

  def abstracted[J >: I](variable:J):Expr[Option[I]] = flatMap(
    j => Iden(Some(j) filterNot(k => k==variable))
  )
}

case class Iden[I](name: I) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = f(name)
}

case class Appl[I](operator: Expr[I], operands: List[Expr[I]]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = Appl(operator.flatMap(f), operands.map(_.flatMap(f)))
}

case object Type extends Expr[Nothing] {
  def flatMap[J](f: Nothing => Expr[J]) = this
}

//Shield a term from certain substitutions
case class Shift[I](expr: Expr[I]) extends Expr[Option[I]] {
  override def flatMap[J](f: (Option[I]) => Expr[J]): Expr[J] = expr.flatMap(i => f(Some(i)))
}

//Using the None of Option as fresh variable symbol has some advantages.
//Adjust to lists of sorts...
case class Abst[I](sort: Expr[I], conc: Expr[Option[I]]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = Abst(sort.flatMap(f), conc.flatMap {
    case Some(i) => Shift(f(i))
    case None => Iden(None)
  })
}

case class Prod[I](typeFunction: Abst[I]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = Prod(typeFunction.flatMap(f))
}
