package nl.woupiestek.dtlang

import scala.collection.mutable

case class Typer[T](value: Context => Option[T]) {
  def ap[U](f: Typer[T => U]): Typer[U] = Typer[U] { context =>
    (f.value(context), value(context)) match {
      case (Some(g), Some(x)) => Some(g(x))
      case _ => None
    }
  }

  def map[U](f: T => U): Typer[U] = Typer { context =>
    value(context).map(f)
  }

  def flatMap[U](f: T => Typer[U]): Typer[U] = Typer { context =>
    value(context).flatMap { t => f(t).value(context) }
  }
}

sealed trait Context {
  def get(index: Int): Option[Expression]

  def getType(index: Int): Option[Expression]

}

object Context {

  case object Empty extends Context {
    override def get(index: Int): Option[Expression] = None

    override def getType(index: Int): Option[Expression] = None
  }

  case class Abstraction(types: Seq[Expression], subcontext: Context) extends Context {
    override def get(index: Int): Option[Expression] =
      if (index < types.length) None
      else subcontext.get(index - types.length)

    override def getType(index: Int): Option[Expression] =
      if (index < types.length) Some(types(index))
      else subcontext.getType(index - types.length)

  }

  case class Grab(annotations: Seq[(Expression, Expression)], subcontext: Context) extends Context {
    override def get(index: Int): Option[Expression] =
      if (index < annotations.length) Some(annotations(index)._1)
      else subcontext.get(index - annotations.length)

    override def getType(index: Int): Option[Expression] =
      if (index < annotations.length) Some(annotations(index)._2)
      else subcontext.getType(index - annotations.length)
  }

}


case class Memoized[X, Y](f: (X => Y) => X => Y) extends (X => Y) {
  private val memory: mutable.Map[X, Y] = mutable.Map.empty

  def apply(x: X): Y = {
    if (memory contains x) memory(x)
    else {
      val y = f(apply)(x)
      memory += (x -> y)
      y
    }
  }
}

object Typer {

  import Expression._

  def infer: Expression => Typer[Expression] = Memoized {
    infer => {
      case Variable(i) => Typer { context => context getType i }
      case Application(y, z) => Typer { context =>
        infer(y).value(context).flatMap {
          case Product(t) => normalize(t, z)
          case _ => None
        }
      }
      case Abstraction(y, z) => Typer { context =>
        infer(z).value(Context.Abstraction(y, context)) map { w => Abstraction(y, w) }
      }
      case _ => Typer { _ => None }
    }
  }

  def grab(operator: Expression, context: Context): Expression = operator match {
    case Variable(i) => context.get(i) getOrElse Variable(i)
    case Application(x, y) => Application(grab(x, context), y.map(grab(_, context)))
    case Abstraction(x, y) => Abstraction(x.map(grab(_, context)), grab(y, Context.Abstraction(x, context)))
    case Product(x) => Product(grab(x, context))
    case Universe => Universe
  }

  def normalize(operator: Expression, operands: Seq[Expression]): Option[Expression] = operator match {
    case Variable(i) => Some(Application(operator, operands))
    case Application(x, y) => normalize(x, y ++ operands)
    case Abstraction(x, y) if x.length <= operands.length =>
      val (head, tail) = operands.reverse.splitAt(x.length)
      val annotations = head zip x.reverse
      if (check(annotations)) normalize(grab(y, Context.Grab(annotations, Context.Empty)), tail)
      else None
    case Abstraction(x, y) if x.length > operands.length =>
      val (head, tail) = x.reverse.splitAt(operands.length)
      val annotations = operands.reverse zip head
      if (check(annotations)) {
        val context = Context.Abstraction(tail.reverse, Context.Grab(annotations, Context.Empty))
        Some(Abstraction(tail, grab(y, context)))
      } else None
    case _ => if (operands.isEmpty) Some(operator) else None
  }

  def check(annotations: Seq[(Expression, Expression)]): Boolean = annotations.forall {
    case (x, y) => infer(x).value(Context.Empty).exists(unify(_, y))
  }

  def unify(x: Expression, y: Expression): Boolean = {
    (normalize(x, Nil), normalize(y, Nil)) match {
      case (Some(Application(x1, x2)), Some(Application(y1, y2))) =>
        unify(x1, y1) && (x2 zip y2).forall { case (x3, y3) => unify(x3, y3) }
      case (Some(Abstraction(x1, x2)), Some(Abstraction(y1, y2))) =>
        unify(x2, y2) && (x1 zip y1).forall { case (x3, y3) => unify(x3, y3) }
      case (Some(Product(x1)), Some(Product(y1))) => unify(x1, y1)
      case (x0, y0) => x0 == y0
    }
  }
}
