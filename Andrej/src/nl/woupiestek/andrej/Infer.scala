package nl.woupiestek.andrej
/**
 * @author Wouter
 */
object Infer {
  type Context = Map[Variable, Expression]

  def apply(context: Context, expression: Expression): Option[Expression] = {
    expression match {
      case x: Variable => context get x
      case Universe(n) => Some(Universe(n + 1))
      case Product(Abstraction(v, d, t)) => {
        for (
          k1 <- inferUniverse(context, d);
          k2 <- inferUniverse(context + (v -> d), t)
        ) yield Universe(Math.max(k1, k2))

      }
      case Abstraction(v, d, t) => {
        for (
          _ <- inferUniverse(context, d);
          te <- apply(context + (v -> d), t)
        ) yield Product(Abstraction(v, d, te))
      }
      case Application(t, u) => {
        for (
          Abstraction(variable, domain, term) <- inferProduct(context, t);
          te <- apply(context, u) if unifiable(Evaluate(domain), Evaluate(te))
        ) yield {
          Evaluate(Cut(variable, u, term))
        }
      }
    }
  }

  private def inferUniverse(context: Context, expression: Expression): Option[Int] = {
    (Infer.apply(context, expression).map { Evaluate(_) }) match {
      case Some(Universe(k)) => Some(k)
      case _                 => None
    }
  }

  private def inferProduct(context: Context, expression: Expression) = {
    (Infer.apply(context, expression).map { Evaluate(_) }) match {
      case Some(Product(k)) => Some(k)
      case _                => None
    }
  }

  def unifiable(e1: Expression, e2: Expression): Boolean = {
    (e1, e2) match {
      case (Application(x, y), Application(x2, y2)) => unifiable(x, x2) && unifiable(y, y2)
      case (Product(x), Product(y)) => unifiable(x, y)
      case (Abstraction(v, d, t), Abstraction(v2, d2, t2)) => unifiable(d, d2) && unifiable(Evaluate(Cut(v, v2, t)), t2)
      case _ => e1 == e2
    }
  }

  private def unifiableAbstraction(a1: Abstraction, a2: Abstraction): Boolean = {
    a1.domain == a2.domain && unifiable(Evaluate(Cut(a1.variable, a2.variable, a1.term)), a2.term)
  }
}