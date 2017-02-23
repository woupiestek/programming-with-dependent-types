package nl.woupiestek.andrej

import nl.woupiestek.andrej.Utils.traverse

trait Expr[E] {
  def identifier(key: String): E

  def let(key: String, value: E, context: E): E

  def application(operator: E, operands: List[E]): E

  def lambda(key: String, dom: E, value: E): E

  def universe: E

  def product(key: String, dom: E, fun: E): E
}

trait DeBruijnExpr[E] {
  def get(index: Int): E

  def push(value: E, context: E): E

  def application(operator: E, operands: List[E]): E

  def lambda(dom: E, value: E): E

  def omega: E

  def pi(dom: E, fun: E): E
}

class StripVars[E](e: DeBruijnExpr[E]) extends Expr[List[String] => Option[E]] {
  override def identifier(key: String): (List[String]) => Option[E] =
    vars => Some(vars.indexOf(key)).collect { case n if n >= 0 => e.get(n) }

  override def let(key: String, value: (List[String]) => Option[E], context: (List[String]) => Option[E]): (List[String]) => Option[E] =
    vars => for {
      x <- value(vars)
      y <- context(key :: vars)
    } yield e.push(x, y)

  override def application(operator: (List[String]) => Option[E], operands: List[(List[String]) => Option[E]]): (List[String]) => Option[E] =
    vars => for {
      x <- operator(vars)
      y <- traverse(operands)(_(vars))
    } yield e.application(x, y)

  override def lambda(key: String, dom: (List[String]) => Option[E], value: (List[String]) => Option[E]): (List[String]) => Option[E] =
    vars => for {
      d <- dom(vars)
      v <- value(key :: vars)
    } yield e.lambda(d, v)

  override def universe: (List[String]) => Option[E] = _ => Some(e.omega)

  //override def product(fun: (List[String]) => Option[E]): (List[String]) => Option[E] = vars => fun(vars).map(e.product)
  override def product(key: String, dom: (List[String]) => Option[E], fun: (List[String]) => Option[E]): (List[String]) => Option[E] =
    vars => for {
      d <- dom(vars)
      v <- fun(key :: vars)
    } yield e.pi(d, v)
}