package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer2 {

  final case class SentenceTask(
      sentence: Sentence,
      values: Heap
  )

  final case class EvaluatedSentence(
      lHead: Value,
      lTail: List[Either[Value, TermTask]],
      rHead: Value,
      rTail: List[Either[Value, TermTask]],
      tail: List[SentenceTask],
      values: List[Value]
  )

  @tailrec def analyze(
      head: Sentence,
      tail: List[SentenceTask],
      values: List[Value]
  ): EvaluatedSentence = head match {
    case Generalization(varName, body) =>
      analyze(
        body,
        tail,
        Value(varName, values.length) :: values
      )
    case Implication(ante, con) =>
      analyze(con, SentenceTask(ante, values) :: tail, values)
    case Equation(left, right) =>
      val heap = values.foldRight(Map.empty[String, Either[Value, TermTask]])(
        (x, y) => y + (x.name -> Left(x))
      )
      val lValue = eval(left, heap, Nil, values)
      val rValue = eval(
        right,
        heap,
        lValue.args
          .take(lValue.args.length - values.length)
          .reverse
          .map(Left(_)),
        lValue.args
      )
      EvaluatedSentence(
        lValue.head,
        lValue.tail ++ rValue.args
          .take(rValue.args.length - lValue.args.length)
          .reverse
          .map(Left(_)),
        rValue.head,
        rValue.tail,
        tail: List[SentenceTask],
        rValue.args
      )
  }

  final case class Value(name: String, offset: Int)

  final case class TermTask(term: Term, heap: Heap)

  type Heap = Map[String, Either[Value, TermTask]]

  trait EvaluatedTerm
  final case class EvaluatedApplication(
      head: Value,
      tail: List[Either[Value, TermTask]]
  ) extends EvaluatedTerm
  final case class EvaluatedAbstraction(
      varName: String,
      body: TermTask
  ) extends EvaluatedTerm

  @tailrec def eval(
      term: Term,
      heap: Heap,
      stack: List[Either[Value, TermTask]],
      args: List[Value]
  ): EvaluatedTerm = term match {
    case Abstraction(varName, body) =>
      stack match {
        case Nil =>
          EvaluatedAbstraction(varName, TermTask(body, heap - varName))
        case h :: t => eval(body, heap + (varName -> h), t, args)
      }
    case Application(operator, operand) =>
      eval(operator, heap, Right(TermTask(operand, heap)) :: stack, args)
    case Let(varName, value, context) =>
      eval(
        context,
        heap + (varName -> Right(TermTask(value, heap))),
        stack,
        args
      )
    case TermVar(name) =>
      heap.get(name) match {
        case None                        => EvaluatedApplication(Value(name, -1), stack)
        case Some(Left(value))           => EvaluatedApplication(value, stack)
        case Some(Right(TermTask(t, h))) => eval(t, h, stack, args)
      }
  }
}
