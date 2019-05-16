package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer3 {

  final case class Value(name: String, level: Int)
  final case class A(sentence: Sentence, args: List[Value])
  final case class E(
      lOperator: Value,
      lOperands: List[Either[Value, Task]],
      rOperator: Value,
      rOperands: List[Either[Value, Task]]
  )
  final case class Task(term: Term, heap: Heap)
  type Heap = Map[String, Either[Value, Task]]

  final case class Sequent(
      equations: List[E],
      antes: List[Sequent],
      values: List[Value]
  )

  //switching to eager evaluation
  @tailrec def deepAnalysis(
      sentence: Sentence
  ): Sequent = {
    def helper(
        in: List[A],
        out: List[(Sentence, List[E], List[A], List[Value])],
        offset: Int
    ): Sequent = in match {
      case Nil =>
        out.foldLeft(Map.empty[Sentence, Sequent]) {
          case (out, (a, b, c, d)) =>
            out + (a -> Sequent(b, c.map(_.sentence).flatMap(out.get), d))
        }(sentence)
      case A(h0, h1) :: t =>
        val (es, as, vs, o2) = analyze(h0, Nil, h1, offset)
        helper(as ++ t, (h0, es, as, vs) :: out, o2)
    }
    helper(A(sentence, Nil) :: Nil, Nil, 0)
  }

  @tailrec def analyze(
      sentence: Sentence,
      antes: List[A],
      values: List[Value],
      offset: Int
  ): (List[E], List[A], List[Value], Int) = sentence match {
    case Generalization(varName, body) =>
      analyze(
        body,
        antes,
        Value(varName, offset) :: values,
        offset + 1
      )
    case Implication(ante, con) =>
      analyze(con, A(ante, values) :: antes, values, offset)
    case Equation(left, right) =>
      val heap = values.foldRight(Map.empty[String, Either[Value, Task]])(
        (x, y) => y + (x.name -> Left(x))
      )
      val (equations, args, offset2) =
        equate(
          (Right(Task(left, heap)), Right(Task(right, heap))) :: Nil,
          Nil,
          values,
          offset
        )
      (equations, antes, args, offset2)
  }

  @tailrec def equate(
      in: List[(Either[Value, Task], Either[Value, Task])],
      out: List[E],
      args: List[Value],
      offset: Int
  ): (List[E], List[Value], Int) = in match {
    case Nil => (out, args, offset)
    case h :: t =>
      h match {
        case (Right(Task(left, lHeap)), Right(Task(right, rHeap))) =>
          val (lOperator, lOperands, lArgs, lOffset) =
            eval(left, lHeap, Nil, Nil, offset)
          val (rOperator, rOperands, rArgs, rOffset) =
            eval(right, rHeap, lArgs.map(Left(_)).reverse, Nil, lOffset)
          val lOperands2 = lOperands ++ rArgs.map(Left(_)).reverse
          if (lOperator == rOperator && lOperands2.length == rOperands.length) {
            equate(
              lOperands2.zip(rOperands) ++ t,
              out,
              rArgs ++ lArgs ++ args,
              rOffset
            )
          } else {
            equate(
              t,
              E(lOperator, lOperands2, rOperator, rOperands) :: out,
              rArgs ++ lArgs ++ args,
              rOffset
            )
          }
        case (Right(Task(left, lHeap)), Left(rOperator)) =>
          val (lOperator, lOperands2, lArgs, lOffset) =
            eval(left, lHeap, Nil, Nil, offset)
          val rOperands = lArgs.map(Left(_)).reverse
          if (lOperator == rOperator && lOperands2.length == rOperands.length) {
            equate(lOperands2.zip(rOperands) ++ t, out, lArgs ++ args, lOffset)
          } else {
            equate(
              t,
              E(lOperator, lOperands2, rOperator, rOperands) :: out,
              lArgs ++ args,
              lOffset
            )
          }
        case (Left(rOperator), Right(Task(left, lHeap))) =>
          val (lOperator, lOperands2, lArgs, lOffset) =
            eval(left, lHeap, Nil, Nil, offset)
          val rOperands = lArgs.map(Left(_)).reverse
          if (lOperator == rOperator && lOperands2.length == rOperands.length) {
            equate(lOperands2.zip(rOperands) ++ t, out, lArgs ++ args, lOffset)
          } else {
            equate(
              t,
              E(lOperator, lOperands2, rOperator, rOperands) :: out,
              lArgs ++ args,
              lOffset
            )
          }
        case (Left(lOperator), Left(rOperator)) =>
          equate(t, E(lOperator, Nil, rOperator, Nil) :: out, args, offset)
      }
  }

  @tailrec def eval(
      term: Term,
      heap: Heap,
      stack: List[Either[Value, Task]],
      args: List[Value],
      offset: Int
  ): (Value, List[Either[Value, Task]], List[Value], Int) = term match {
    case Abstraction(varName, body) =>
      stack match {
        case Nil =>
          val v = Value(varName, offset)
          eval(body, heap + (varName -> Left(v)), Nil, v :: args, offset + 1)
        case h :: t => eval(body, heap + (varName -> h), t, args, offset)
      }
    case Application(operator, operand) =>
      eval(operator, heap, Right(Task(operand, heap)) :: stack, args, offset)
    case Let(varName, value, context) =>
      eval(
        context,
        heap + (varName -> Right(Task(value, heap))),
        stack,
        args,
        offset
      )
    case TermVar(name) =>
      heap.get(name) match {
        case None                    => (Value(name, -1), stack, args, offset)
        case Some(Left(value))       => (value, stack, args, offset)
        case Some(Right(Task(t, h))) => eval(t, h, stack, args, offset)
      }
  }

}
