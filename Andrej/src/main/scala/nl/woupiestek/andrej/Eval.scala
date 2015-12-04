package nl.woupiestek.andrej

//normalization using a simple Krivine Machine
object WeakHeadNormalize {
  def apply[I](expr: Expr[I], cont: List[Expr[I]] = Nil): Expr[I] = (expr, cont) match {
    case (Appl(x, y), z) => apply[I](x, y ++ z)
    case (Abst(sort, conc), head :: tail) => apply[I](conc @: head, tail)
    case (x, y) => Appl(x, y)
  }
}

case class Infi[I](sort: Expr[I]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]): Expr[J] = Infi(sort.flatMap(f))
}

object Infer {
  def unifiable[I](x: Expr[I], y: Expr[I]): Boolean = {
    val nx = WeakHeadNormalize(x, Nil)
    val ny = WeakHeadNormalize(y, Nil)
    (nx, ny) match {
      case (Appl(a, b), Appl(c, d)) => unifiable(a, c) && (b zip d).forall { case (e, f) => unifiable(e, f) }
      case (Abst(a, b), Abst(c, d)) => unifiable(a, c) && unifiable(b, d)
      case (Prod(a), Prod(b)) => unifiable(a, b)
      case (Shift(a), Shift(b)) => unifiable(a, b)
      case _ => nx == ny //remaining cases don't require further normalization.
    }
  }

  def apply[I](expr: Expr[I]): Option[Expr[I]] = expr match {
    case Appl(x, y) => inferInContext(x, y)
    case Infi(sort) => Some(sort)
    case Abst(t, x) => apply(x @: Infi(t)) map { x => Prod(Abst(t, Shift(x))) }
    case Prod(_) => Some(Type)
    case Type => Some(Type)
  }

  def inferInContext[I](expr: Expr[I], stack: List[Expr[I]]): Option[Expr[I]] = (expr, stack) match {
    case (Appl(x, y), z) => inferInContext(x, y ++ z)
    case (Abst(t, x), head :: tail) => guardedCombine(t, x, head) flatMap { x => inferInContext(x, tail) }
    case (x, Nil) => apply(x)
    case _ => None
  }

  def guardedNormalize[I]: (Expr[I], List[Expr[I]]) => Option[Expr[I]] = {
    case (Appl(x, y), tail) => guardedNormalize(x, y ++ tail)
    case (Abst(sort, conc), head :: tail) =>
      guardedCombine(sort, conc, head) flatMap { x => guardedNormalize[I](x, tail) }
    case (x, y) => Some(Appl(x, y))
  }

  def guardedCombine[I](t: Expr[I], x: Expr[Option[I]], y: Expr[I]): Option[Expr[I]] = {
    for (s <- apply(y) if unifiable(s, t)) yield x @: y
  }
}

