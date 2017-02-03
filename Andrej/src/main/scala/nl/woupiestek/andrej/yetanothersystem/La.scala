package nl.woupiestek.andrej.yetanothersystem

/**
 * Created by Wouter on 23-1-2017.
 */
sealed trait La {
  def cut(i: Int, x: La): La

  def app(x: La): La
}

case class App(args: List[La], index: Int) extends La {
  override def cut(i: Int, x: La): La = {
    val head = if (index > i) App(Nil, index - 1) else if (index == i) x else App(Nil, index)
    args.foldRight(head) { case (y, z) => z.app(y.cut(i, x)) }
  }

  override def app(x: La): La = App(x :: args, index)
}

case class Lam(body: La) extends La {
  override def cut(i: Int, x: La): La = Lam(body.cut(i + 1, x))

  override def app(x: La): La = body.cut(0, x)
}

sealed trait TA {
  def push(x: List[TA]): TA

  def app(x: TA): TA
}

case class TApp(args: List[TA], index: Int) extends TA {
  override def push(x: List[TA]): TA = {
    val y = if (index < x.length) x(index) else TApp(Nil, index - x.length)
    args.foldRight(y) { case (a, b) => b.app(a.push(x)) }
  }

  override def app(x: TA): TA = TApp(x :: args, index)
}

case class TAbs(context: List[TA], body: TA) extends TA {
  override def push(x: List[TA]): TA = TAbs(x ++ context.map(_.push(x)), body)

  override def app(x: TA): TA = body.push(x :: context)
}