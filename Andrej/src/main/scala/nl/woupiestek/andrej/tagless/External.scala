package nl.woupiestek.andrej.tagless

trait External[T] {

  def variable(name: String): T

  def application(operator: T, operand: T): T

  def lambda(name: String, typ: T, body: T): T

  def product: T

  def universe: T

}

trait Internal[T] {

  def variable(index: Int): T

  def substitution(context: List[T], body: T): T

  def universe: T

  def product: T

  def lambda(typ: T, body: T): T

  def application(operator: T, operand: T): T
}

class Internalize[T](e: Internal[T]) extends External[List[String] => Option[T]] {

  override def variable(name: String): (List[String]) => Option[T] =
    vars => Some(vars.indexOf(name)).filter(_ >= 0).map(e.variable)

  override def application(operator: (List[String]) => Option[T], operand: (List[String]) => Option[T]): (List[String]) => Option[T] =
    vars => for {
      x <- operator(vars)
      y <- operand(vars)
    } yield e.application(x, y)

  override def lambda(name: String, typ: (List[String]) => Option[T], body: (List[String]) => Option[T]): (List[String]) => Option[T] =
    vars => for {
      t <- typ(vars)
      b <- body(name :: vars)
    } yield e.lambda(t, b)

  override def universe: (List[String]) => Option[T] = _ => Some(e.universe)

  override def product: (List[String]) => Option[T] = _ => Some(e.product)
}

sealed trait LambdaImpl

object LambdaImpl {

  case class Abs(context: List[LambdaImpl], dom: LambdaImpl, body: LambdaImpl) extends LambdaImpl

  case class App(args: List[LambdaImpl], index: Int) extends LambdaImpl

  case object Universe extends LambdaImpl

  case class Product(arg: LambdaImpl) extends LambdaImpl

  case object Fail extends LambdaImpl

  object Eval extends Internal[LambdaImpl] {
    override def variable(index: Int): LambdaImpl = App(Nil, index)

    override def universe: LambdaImpl = Universe

    override def product: LambdaImpl = lambda(Universe,lambda(variable(0),Product(variable(0))))

    override def lambda(typ: LambdaImpl, body: LambdaImpl): LambdaImpl = Abs(Nil, typ, body)

    def unifies(f: LambdaImpl, g: LambdaImpl): Boolean = (f, g) match {
      case (Universe, Universe) => true
      case (Product(f2), Product(g2)) => unifies(f2, g2)
      case (Abs(c, d, b), Abs(c2, d2, b2)) =>
        unifies(substitution(c.reverse, d), substitution(c2.reverse, d2)) &&
          unifies(substitution(variable(0) :: c.reverse, d), substitution(variable(0) :: c2.reverse, d2))
      case (App(as, i), App(bs, j)) =>
        i == j && as.length == bs.length && as.zip(bs).forall { case (a, b) => unifies(a, b) }
      case _ => false
    }

    def of(operand: LambdaImpl, typ: LambdaImpl): Boolean = unifies(typeOf(Nil, operand), typ)

    def typeOf(context: List[LambdaImpl], body: LambdaImpl): LambdaImpl = body match {
      case App(args, index) => context lift index match {
        case Some(Product(f)) => args.foldRight(f) { case (x, y) => application(y, x) }
        case _ => Fail
      }
      case Abs(c, d, b) =>
        val c2 = c.map(typeOf(context, _)).reverse
        val d2 = substitution(c, d)
        val b2 = typeOf(d2 :: c2 ++ context, b)
        Product(Abs(Nil, d2, b2))
      case Product(f) => typeOf(context, f) match {
        case Product(Abs(_, _, Universe)) => Universe
        case _ => Fail
      }
      case _ => Fail
    }

    override def application(operator: LambdaImpl, operand: LambdaImpl): LambdaImpl = operator match {
      case Abs(context, typ, body) if of(operand, typ) => substitution(operand :: context.reverse, body)
      case App(args, index) => App(operand :: args, index)
      case _ => Fail
    }

    override def substitution(context: List[LambdaImpl], body: LambdaImpl): LambdaImpl = body match {
      case App(args, index) => context.lift(index).fold[LambdaImpl](Fail) {
        operator => args.foldRight(operator)((x, y) => application(y, x))
      }
      case Abs(context2, typ, body2) => Abs(context2.reverse ++ context, typ, body2)
      case Product(arg) => Product(substitution(context, arg))
      case _ => body
    }
  }

}