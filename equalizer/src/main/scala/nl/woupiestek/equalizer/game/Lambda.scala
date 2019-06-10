package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

sealed abstract class Lambda
object Lambda {
  final case class Pattern(operator: String, operands: List[Lambda])
      extends Lambda
  final case class Closure(term: Term, heap: Map[String, Lambda]) extends Lambda

  @tailrec def evaluate(
      term: Term,
      heap: Map[String, Lambda] = Map.empty,
      stack: List[Lambda] = Nil
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
        case None                => Pattern(varName, stack)
      }
  }

  def prettyPrint(lambda: Lambda): String = lambda match {
    case Lambda.Pattern(operator, operands) =>
      (operator :: operands.map(prettyPrint)).mkString("(", " ", ")")
    case Lambda.Closure(term, heap) =>
      val h = heap
        .map { case (k, v) => s"$k:${prettyPrint(v)}" }
        .mkString(",")
      s"(${Term.prettyPrint(term)}|$h)"
  }

  def free(lambda: Lambda): Set[String] = {
    @annotation.tailrec
    def helper(lambdas: List[Lambda], out: Set[String]): Set[String] =
      lambdas match {
        case Nil => out
        case h :: t =>
          h match {
            case Closure(term, heap) =>
              helper(t, Term.free(term) -- heap.keySet ++ out)
            case Pattern(operator, operands) =>
              helper(operands ++ lambdas, out + operator)
          }
      }
    helper(lambda :: Nil, Set.empty)
  }

}
