package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer {

  final case class Value(name: String, offset: Int)

  type Sequent = (Boolean, Int, List[(Int, List[Value])], List[Value])

  type Pattern = (Value, List[Task])

  type Result = (List[(Pattern, Pattern, Sequent)], Int)

  def analyze(sentence: Sentence): Result =
    analyze(
      (sentence, Map.empty[String, Task], (true, 0, Nil, Nil)) :: Nil,
      Nil,
      1,
      0
    )

  @tailrec def analyze(
      in: List[(Sentence, Map[String, Task], Sequent)],
      out: List[(Pattern, Pattern, Sequent)],
      offset: Int,
      varOffset: Int
  ): Result = in match {
    case Nil => (out, offset)
    case (h, heap, (k, i, j, vars)) :: t =>
      h match {
        case Equation(left, right) =>
          val lValue = evaluate(left, heap, Nil, Nil, varOffset)
          val rValue = evaluate(
            right,
            heap,
            lValue.args.reverse.map(ValueTask(_)),
            lValue.args,
            lValue.varOffset
          )
          analyze(
            t,
            (
              (lValue.operator, lValue.operands),
              (rValue.operator, rValue.operands),
              (k, i, j, rValue.args ++ vars)
            ) :: out,
            offset,
            rValue.varOffset
          )
        case Implication(ante, con) =>
          analyze(
            (ante, heap, (k, i, (offset, vars) :: j, vars)) :: (
              con,
              heap,
              (!k, offset, Nil, Nil)
            ) :: t,
            out,
            offset + 1,
            varOffset
          )
        case Generalization(varName, body) =>
          val value = Value(varName, varOffset)
          analyze(
            (
              body,
              heap + (varName -> ValueTask(value)),
              (k, i, j, value :: vars)
            ) :: t,
            out,
            offset,
            varOffset
          )
      }
  }

  sealed trait Task
  final case class TermTask(term: Term, heap: Map[String, Task]) extends Task
  final case class ValueTask(value: Value) extends Task

  final case class Evaluated(
      operator: Value,
      operands: List[Task],
      args: List[Value],
      varOffset: Int
  )

  @tailrec def evaluate(
      term: Term,
      heap: Map[String, Task],
      stack: List[Task],
      args: List[Value],
      varOffset: Int
  ): Evaluated = term match {
    case Application(operator, operand) =>
      evaluate(
        operator,
        heap,
        TermTask(operand, heap) :: stack,
        args,
        varOffset
      )
    case Abstraction(varName, body) =>
      stack match {
        case Nil =>
          val value = Value(varName, varOffset)
          evaluate(
            body,
            heap + (varName -> ValueTask(value)),
            Nil,
            value :: args,
            varOffset + 1
          )
        case h :: t =>
          evaluate(body, heap + (varName -> h), t, args, varOffset)
      }
    case Let(varName, value, body) =>
      evaluate(
        body,
        heap + (varName -> TermTask(value, heap)),
        stack,
        args,
        varOffset
      )
    case TermVar(varName) =>
      heap.get(varName) match {
        case Some(TermTask(t, h)) =>
          evaluate(t, h, stack, args, varOffset)
        case Some(ValueTask(value)) => Evaluated(value, stack, args, varOffset)
        case None =>
          Evaluated(Value(varName, -1), stack, args, varOffset)
      }
  }

}
