package nl.woupiestek.andrej.lispy

trait Expr

case class Var(name: String) extends Expr

case class Let(key: Var, value: Expr, body: Expr) extends Expr

case class Product(entries: Map[String, Expr]) extends Expr

case class Select(key: Var, index: String, cont: Expr)
    extends Expr

case class Lambda(key: Var, body: Expr) extends Expr

case class Apply(key: Var, arg: Expr, cont: Expr) extends Expr
