package nl.woupiestek.andrej.parser

import nl.woupiestek.andrej.DeBruijnExpr

object PrintDTL extends DeBruijnExpr[List[String] => String] {
  override def get(index: Int): (List[String]) => String = _ lift index getOrElse "?"

  override def push(value: (List[String]) => String, context: (List[String]) => String): (List[String]) => String =
    vars => {
      val xL = "x" + vars.length
      s"[$xL = ${value(vars)}]${context(xL :: vars)}"
    }

  override def application(operator: (List[String]) => String, operands: List[(List[String]) => String]): (List[String]) => String =
    vars => s"(${operator(vars)} ${operands.map(_(vars)).mkString(" ")})"

  override def lambda(dom: (List[String]) => String, value: (List[String]) => String): (List[String]) => String =
    vars => {
      val xL = "x" + vars.length
      s"\\$xL:${dom(vars)}.${value(xL :: vars)}"
    }

  override def omega: (List[String]) => String = _ => "type"

  override def pi(dom: (List[String]) => String, fun: (List[String]) => String): (List[String]) => String =
    vars => {
      val xL = "x" + vars.length
      s"($xL:${dom(vars)}) -> ${fun(xL :: vars)}"
    }
}
