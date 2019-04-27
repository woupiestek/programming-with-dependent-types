package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

final case class Value(name: String, position: Int)

object Analyzer {

  @tailrec def headNormalize(
      sentence: Sentence,
      properties: List[Sentence],
      values: List[Value]
  ): HeadNormalized =
    sentence match {
      case Generalization(varName, con) =>
        headNormalize(con, properties, Value(varName, values.length) :: values)
      case Implication(a, b) =>
        headNormalize(b, a :: properties, values)
      case Equation(m, n) =>
        val lheap: Map[String, Task] =
          values.foldRight(Map.empty[String, Task])(
            (x: Value, y: Map[String, Task]) => y + (x.name -> ValueTask(x))
          )
        val Evaluated(Pattern(loperator, loperands), largs) =
          evaluate(m, lheap, Nil, Nil, values.length)

        val Evaluated(rpattern, rargs) = evaluate(
          n,
          lheap,
          largs.map(ValueTask(_)).reverse,
          Nil,
          values.length + largs.length
        )

        HeadNormalized(
          Pattern(loperator, loperands ++ rargs.map(ValueTask(_)).reverse),
          rpattern,
          properties,
          rargs ++ largs ++ values
        )
    }

  final case class HeadNormalized(
      left: Pattern,
      right: Pattern,
      properties: List[Sentence],
      args: List[Value]
  )

  sealed abstract class Task
  final case class TermTask(term: Term, heap: Map[String, Task]) extends Task
  final case class ValueTask(value: Value) extends Task

  final case class Pattern(operator: Value, operands: List[Task])

  final case class Evaluated(
      pattern: Pattern,
      args: List[Value]
  )
  @tailrec def evaluate(
      term: Term,
      heap: Map[String, Task],
      stack: List[Task],
      values: List[Value],
      offset: Int
  ): Evaluated = term match {
    case Application(operator, operand) =>
      evaluate(
        operator,
        heap,
        TermTask(operand, heap) :: stack,
        values,
        offset
      )
    case Abstraction(varName, body) =>
      stack match {
        case Nil =>
          val v = Value(varName, values.length + offset)
          evaluate(
            body,
            heap + (varName -> ValueTask(v)),
            Nil,
            v :: values,
            offset
          )
        case h :: t =>
          evaluate(body, heap + (varName -> h), t, values, offset)
      }
    case Let(varName, value, body) =>
      evaluate(
        body,
        heap + (varName -> TermTask(value, heap)),
        stack,
        values,
        offset
      )
    case TermVar(varName) =>
      heap.get(varName) match {
        case Some(TermTask(t, h)) =>
          evaluate(t, h, stack, values, offset)
        case Some(ValueTask(v)) =>
          Evaluated(Pattern(v, stack), values)
        case None => //forgotten to bind... could be treated as error
          Evaluated(Pattern(Value(varName, -1), stack), values)
      }
  }

}
