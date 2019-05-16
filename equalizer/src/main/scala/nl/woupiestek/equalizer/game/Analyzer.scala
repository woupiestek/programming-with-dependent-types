package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer {

  final case class Value(name: String, offset: Int)

  type Sequent = (Int, Set[Int], Set[Value])

  def analyze(sentence: Sentence): Sequ =
    analyze(
      (sentence, Map.empty[String, Task], (0, Set.empty[Int], Set.empty[Value])) :: Nil,
      Nil,
      1,
      0
    )

  @tailrec def analyze(
      in: List[(Sentence, Map[String, Task], Sequent)],
      out: List[(Task, Task, Sequent)],
      offset: Int,
      varOffset: Int
  ): Sequ = in match {
    case Nil => catalyze(out, Map.empty)
    case (h, heap, (i, j, vars)) :: t =>
      h match {
        case Equation(left, right) =>
          analyze(
            t,
            (TermTask(left, heap), TermTask(right, heap), (i, j, vars)) :: out,
            offset,
            varOffset
          )
        case Implication(ante, con) =>
          analyze(
            (ante, heap, (i, j + offset, vars)) :: (
              con,
              heap,
              (offset, Set.empty[Int], Set.empty[Value])
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
              (i, j, vars + value)
            ) :: t,
            out,
            offset,
            varOffset
          )
      }
  }

  final case class Sequ(
      left: Task,
      right: Task,
      ante: Set[Sequ],
      args: Set[Value]
  )

  def catalyze(
      in: List[(Task, Task, Sequent)],
      out: Map[Int, Sequ]
  ): Sequ = in match {
    case Nil => out(0)
    case (p, q, (r, s, t)) :: u =>
      catalyze(
        u,
        out + (r ->
          Sequ(p, q, out.collect { case (i, j) if s(i) => j }.toSet, t))
      )
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
