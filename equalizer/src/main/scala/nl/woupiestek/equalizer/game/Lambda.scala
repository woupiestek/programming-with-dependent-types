package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

sealed abstract class Lambda extends (List[Lambda] => Lambda)
object Lambda {
  final case class Value(name: String, offset: Int)

  final case class Pattern(operator: Value, operands: List[Lambda])
      extends Lambda {
    def apply(lambdas: List[Lambda]): Lambda =
      Pattern(operator, operands ++ lambdas)
  }
  final case class Closure(term: Term, heap: Map[String, Lambda])
      extends Lambda {
    def apply(lambdas: List[Lambda]): Lambda = evaluate(term, heap, lambdas)
  }

  @tailrec def evaluate(
      term: Term,
      heap: Map[String, Lambda],
      stack: List[Lambda]
  ): Lambda = term match {
    case Application(operator, operand) =>
      evaluate(operator, heap, Closure(operand, heap) :: stack)
    case Abstraction(varName, body) =>
      stack match {
        case Nil    => Closure(term, heap)
        case h :: t => evaluate(body, heap + (varName -> h), t)
      }
    case Let(varName, value, body) =>
      evaluate(body, heap + (varName -> Closure(value, heap)), stack)
    case TermVar(varName) =>
      heap.get(varName) match {
        case Some(Closure(t, h)) => evaluate(t, h, stack)
        case Some(Pattern(x, y)) => Pattern(x, y ++ stack)
        case None                => Pattern(Value(varName, -1), stack)
      }
  }
}
