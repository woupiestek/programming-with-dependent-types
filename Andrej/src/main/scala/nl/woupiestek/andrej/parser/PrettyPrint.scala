package nl.woupiestek.andrej.parser

import nl.woupiestek.andrej.DeBruijnExpr

object PrettyPrint extends DeBruijnExpr[List[String] => String] {
  override def get(index: Int): (List[String]) => String = _ lift index getOrElse "?"

  override def push(value: (List[String]) => String, context: (List[String]) => String): (List[String]) => String =
    vars => {
      val xL = "x" + vars.length
      s"(let $xL be ${value(vars)} in ${context(xL :: vars)})"
    }

  override def application(operator: (List[String]) => String, operand: (List[String]) => String): (List[String]) => String =
    vars => s"(${operator(vars)} ${operand(vars)})"

  override def lambda(dom: (List[String]) => String, value: (List[String]) => String): (List[String]) => String =
    vars => {
      val xL = "x" + vars.length
      s"(\\$xL:${dom(vars)}.${value(xL :: vars)})"
    }

  override def universe: (List[String]) => String = _ => "type"

  override def product(dom: (List[String]) => String, fun: (List[String]) => String): (List[String]) => String =
    vars => {
      val xL = "x" + vars.length
      s"(pi $xL:${dom(vars)}.${fun(xL :: vars)})"
    }
}
