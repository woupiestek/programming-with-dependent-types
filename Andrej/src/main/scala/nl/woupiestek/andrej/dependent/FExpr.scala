package nl.woupiestek.andrej.dependent

trait FExpr[T] {
  def get(index: Int): T

  def push(value: T, cont: T): T

  def function(dom: T, cont: T): T

  def deFunction(index: Int, argument: T, cont: T): T

  def product(dom: T, cont: T): T

  def omega: T
}

sealed trait FTask

case object FFail extends FTask

case object FOmega extends FTask

case class FGet(index: Int) extends FTask

case class FClosure(dom: FTask, con: FTask) extends FTask

case class FProduct(dom: FTask, con: FTask) extends FTask

case class FDefunction(index: Int, arg: FTask, cont: FTask) extends FTask

object Typing extends FExpr[List[FTask] => FTask] {
  override def get(index: Int): (List[FTask]) => FTask = _ lift index getOrElse FFail

  override def push(value: (List[FTask]) => FTask, cont: (List[FTask]) => FTask): (List[FTask]) => FTask =
    list => cont(value(list) :: list)

  override def function(dom: (List[FTask]) => FTask, cont: (List[FTask]) => FTask): (List[FTask]) => FTask =
    list => {
      val d = dom(list)
      FProduct(d, cont(d :: list))
    }

  override def product(dom: (List[FTask]) => FTask, cont: (List[FTask]) => FTask): (List[FTask]) => FTask =
    list => if (FOmega == cont(dom(list) :: list)) FOmega else FFail

  override def omega: (List[FTask]) => FTask = _ => FFail

  override def deFunction(index: Int, argument: (List[FTask]) => FTask, cont: (List[FTask]) => FTask): (List[FTask]) => FTask =
    list => get(index)(list) match {
      case FProduct(c, d) if argument(list) == c => cont(d :: list)
      case _ => FFail
    }
}

object Valuing extends FExpr[FTask] {
  override def get(index: Int): FTask = FGet(index)

  private def push(value: FTask, cont: FTask, depth: Int): FTask = cont match {
    case FFail => FFail
    case FOmega => FOmega
    case FGet(n) if n < depth => cont
    case FGet(n) if n == depth => value
    case FGet(n) if n > depth => FGet(n - 1)
    case FClosure(d, c) => FClosure(push(value, d, depth), push(value, c, depth + 1))
    case FProduct(d, c) => FProduct(push(value, d, depth), push(value, c, depth + 1))
    case FDefunction(i, a, c) if i < depth => FDefunction(i, push(value, a, depth), push(value, c, depth + 1))
    case FDefunction(i, a, c) if i == depth => value match {
      case FClosure(_, b) => push(push(a, b), c, i)
      case _ => FFail
    }
    case FDefunction(i, a, c) if i > depth => FDefunction(i - 1, push(value, a, depth), push(value, c, depth + 1))
  }

  override def function(dom: FTask, cont: FTask): FTask = FClosure(dom, cont)

  override def deFunction(index: Int, argument: FTask, cont: FTask): FTask =
    FDefunction(index, argument, cont)

  override def product(dom: FTask, cont: FTask): FTask = FProduct(dom, cont)

  override def omega: FTask = FOmega

  override def push(value: FTask, cont: FTask): FTask = push(value, cont, 0)
}
