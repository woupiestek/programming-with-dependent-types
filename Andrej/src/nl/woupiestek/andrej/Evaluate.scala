package nl.woupiestek.andrej

/**
 * @author Wouter
 */
object Evaluate {

  def apply(expression: Expression): Expression = {
    expression match {
      case Application(t, u) => {
        val u2 = apply(u)
        apply(t) match {
          case Abstraction(v, d, t3) => apply(Cut(v, u, t3))
          case v                     => Application(v, u)
        }
      }
      case Cut(v, t, u) => apply(cut(v, apply(t), apply(u)))
      case x            => x
    }
  }

  private def cut(variable: Variable, left: Expression, right: Expression): Expression = {
    right match {
      case `variable`        => left
      case Application(t, u) => Application(cut(variable, left, t), cut(variable, left, u))
      case Abstraction(v, d, t) => {
        if (v == variable) cut(Masked(variable), left, right)
        else Abstraction(v, cut(variable, left, d), cut(variable, Cut(v, Masked(v), left), t))
      }
      case Cut(v, l, r) => {
        if (v == variable) cut(Masked(variable), left, right)
        else Cut(v, cut(variable, left, l), cut(variable, Cut(v, Masked(v), left), r))
      }
      case x => x
    }
  }

  private case class Masked(variable: Variable) extends Variable

}