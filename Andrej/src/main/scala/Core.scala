package nl.woupiestek.andrej.core2

/**
 * Created by Wouter on 28-8-2015.
 */
object Core {

}


// terms should form a sort of free monad
// hence the genericity and flatMap operators down here
trait Expr[+I] {
  def flatMap[J](f: I => Expr[J]): Expr[J]

  def map[J](f: I => J): Expr[J] = flatMap(i => Iden(f(i)))
}

case class Iden[I](identifier: I) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = f(identifier)
}

case class Appl[I](operator: Expr[I], operand: Expr[I]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = Appl(operator.flatMap(f), operand.flatMap(f))
}

case class Type(level: Int) extends Expr[Nothing] {
  def flatMap[J](f: (Nothing) => Expr[J]) = this
}

//Using the None of Option as fresh variable symbol has some advantages.
case class Abst[I](sort: Expr[I], conc: Expr[Option[I]]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]) = Abst(sort.flatMap(f), conc.flatMap {
    case Some(i) => f(i).map(Some(_))
    case None => Iden(None)
  })
}

case class Prod[I](typeFunction: Abst[I]) extends Expr[I] {
  def this(sort: Expr[I], conc: Expr[Option[I]]) = this(Abst(sort, conc))

  def flatMap[J](f: I => Expr[J]) = Prod(typeFunction.flatMap(f))
}

//normalization using a simple Krivine Machine
object WeakHeadNormalize {
  def apply[I]: (Expr[I], List[Expr[I]]) => Expr[I] = {
    case (Appl (x, y), tail) => apply (x, y :: tail)
    case (Abst (sort, conc), head :: tail) => apply[I] (combine (conc, head), tail)
    case (expr, stack) => stack.foldLeft (expr) {(x, y) => Appl (x, y)}
  }

  def combine[I](operator: Expr[Option[I]], operand: Expr[I]) = operator.flatMap {
    case None => operand
    case Some(iden) => Iden(iden)
  }
}

//solution for type inference problem: forbid empty types.
case class Infi[I](sort: Expr[I]) extends Expr[I] {
  def flatMap[J](f: I => Expr[J]): Expr[J] = Infi(sort.flatMap(i => f(i)))
}

object Infer {
  def unifiable[I](x: Expr[I], y: Expr[I]): Boolean = {
    val nx = WeakHeadNormalize(x,Nil)
    val ny = WeakHeadNormalize(y,Nil)
    (nx, ny) match {
      case (Appl(a, b), Appl(c, d)) => unifiable(a, c) && unifiable(b, d)
      case (Abst(a, b), Abst(c, d)) => unifiable(a, c) && unifiable(b, d)
      case (Prod(a), Prod(b)) => unifiable(a, b)
      case _ => nx == ny //remaining cases don't require further normalization.
    }
  }

  //dynamic type inference.
  //maybe adding a context is better than working with Dummys
  //we need to adjust the context to a shift to Option[I]
  //
  def apply[I](expr: Expr[I], stack: List[Expr[I]] = Nil): Option[Expr[I]] =
    (expr, stack) match {
      case (Infi(sort), _) => guardedNormalize(sort, stack)
      case (Type(n), Nil) => Some(Type(n + 1))
      case (Appl(x, y), z) => apply(x, y :: z)
      case (Abst(t, x), head :: tail) => for (
        xhead <- guardedCombine(t, x, head);
        z <- apply(xhead, tail)
      ) yield z
      case (Abst(t, x), Nil) => for (
        xType <- apply(WeakHeadNormalize.combine(x, Infi(t)));
      ) yield Prod(Abst(t, xType.map(Some(_))))
      case (Prod(sort, conc), Nil) => for (
        type1 <- apply(sort);
        Type(level1) <- guardedNormalize(type1, Nil);
        type2 <- apply(conc);
        Type(level2) <- guardedNormalize(type2, Nil)
      ) yield Type(Math.max(level1, level2))
      case _ => None
    }

  def guardedNormalize[I]: (Expr[I], List[Expr[I]]) => Option[Expr[I]] = {
    case (Appl(x, y), tail) => guardedNormalize(x, y :: tail)
    case (Abst(sort, conc), head :: tail) => for (
      x <- guardedCombine(sort, conc, head);
      y <- guardedNormalize[I](x, tail)
    ) yield y
    case (expr, stack) => Some(stack.foldLeft(expr) { (x, y) => Appl(x, y) })
  }

  def guardedCombine[I](t: Expr[I], x: Expr[Option[I]], head: Expr[I]): Option[Expr[I]] = {
    for (
      s <- apply(head) if unifiable(s, t);
      r <- WeakHeadNormalize.combine(x, head)
    ) yield r
  }
}
