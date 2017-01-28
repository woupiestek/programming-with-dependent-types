package nl.woupiestek.andrej.dependent

trait Lambda

case class LGet(index: Int) extends Lambda

case class LClosure(dom: Lambda, fun: Lambda => Lambda) extends Lambda

case class LProduct(dom: Lambda, fun: Lambda => Lambda) extends Lambda

case object LFail extends Lambda

case object LOmega extends Lambda

object LambdaValuing extends FExpr[List[Lambda] => Lambda] {
  override def get(index: Int): (List[Lambda]) => Lambda = _ lift index getOrElse LFail

  override def push(value: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => cont(value(list) :: list)

  override def function(dom: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => LClosure(dom(list), x => cont(x :: list))

  override def deFunction(index: Int, argument: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => get(index)(list) match {
      case LClosure(_, c) => c(argument(list) :: list)
      case _ => LFail
    }

  override def product(dom: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => LProduct(dom(list), x => cont(x :: list))

  override def omega: (List[Lambda]) => Lambda = _ => LOmega
}


object LambdaTyping extends FExpr[List[Lambda] => Lambda] {
  override def get(index: Int): (List[Lambda]) => Lambda =
    list => if (index < list.length) list(index) else LGet(index - list.length)

  override def push(value: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => cont(value(list) :: list)

  override def function(dom: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => LProduct(dom(list), x => cont(x :: list))

  def unifies(x: Lambda, y: Lambda): Boolean = (x, y) match {
    case (LOmega, LOmega) => true
    case (LGet(i), LGet(j)) => i == j
    case (LClosure(a, b), LClosure(c, d)) => unifies(a, c) && unifies(b(LGet(0)), d(LGet(0)))
    case (LProduct(a, b), LProduct(c, d)) => unifies(a, c) && unifies(b(LGet(0)), d(LGet(0)))
    case _ => false
  }

  override def deFunction(index: Int, argument: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => get(index)(list) match {
      case LProduct(c, d) if unifies(argument(list), c) => cont(d :: list)
      case _ => LFail
    }

  override def product(dom: (List[Lambda]) => Lambda, cont: (List[Lambda]) => Lambda): (List[Lambda]) => Lambda =
    list => if (cont(dom(list) :: list) == LOmega) LOmega else LFail

  override def omega: (List[Lambda]) => Lambda = _ => LFail
}