package nl.woupiestek.andrej.yetanothersystem

sealed trait Expression

case class Axiom(index: Int) extends Expression

case class Abstraction(types: Seq[Expression], expression: Expression) extends Expression

case class Application(operator: Expression, operands: Seq[Expression]) extends Expression

case class Composition(head: Expression, tail: Seq[Expression]) extends Expression

case class TypeOf(expression: Expression) extends Expression

class Machine(operands: Seq[Expression]) {
  def eval(operator: Expression): Expression = operator match {
    case Abstraction(ts, e) if operands.length >= ts.length =>
      val (head, tail) = operands.splitAt(ts.length)
      val composed = new Environment(head, 0).eval(e)
      new Machine(tail).eval(composed)
    case Abstraction(ts, e) if operands.length < ts.length =>
      val (head, tail) = ts.splitAt(operands.length)
      new Environment(operands, 0).eval(Abstraction(tail, e))
    case Application(x, ys) => new Machine(ys ++ operands).eval(x)
    case Composition(head, tail) => eval(new Environment(tail, 0).eval(head))
    case a => Application(a, operands)
  }

}

class Environment(context: Seq[Expression] = Nil, offset: Int) {
  def eval(expression: Expression): Expression = expression match {
    case Axiom(i) => (context lift i) getOrElse Axiom(i - context.length)
    case Abstraction(ts, e) => Abstraction(ts.map(eval), new Environment(context, offset + ts.length).eval(e))
    case Application(x, ys) => new Machine(ys).eval(x)
    case Composition(x, ys) => new Environment(ys.map(eval), 0).eval(eval(x))
    case x => x
  }
}
