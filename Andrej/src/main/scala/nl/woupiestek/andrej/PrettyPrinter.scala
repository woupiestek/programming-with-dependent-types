package nl.woupiestek.andrej

/**
 * Created by Wouter on 4-9-2015.
 */
object PrettyPrint {
  def apply[I](expr: Expr[I]):String = {
    expr match {
      case Appl(x, y) => (x :: y) map apply mkString("(", " ", ")") //TODO: proper bracketing
      case Abst(t, e) => "\\%s:%s.%s".format("&" + apply(t), apply(t), apply(e @: Infi(t))) //TODO: generate variable names
      case Prod(Abst(t, e)) => "forall %s:%s.%s".format("&" + apply(t), apply(t), apply(e @: Infi(t))) //TODO: generate variable names
      case Infi(i) => "&" + apply(i)
      case Shift(e) => "[" + apply(e) + "]"
      case Iden(i) => i toString
      case _ => expr toString
    }
  }
}
