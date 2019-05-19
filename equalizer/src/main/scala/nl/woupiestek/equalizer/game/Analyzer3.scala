package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer3 {

  final case class Value(name: String, level: Int)
  final case class A(sentence: Sentence, args: List[Value])
  final case class Task(term: Term, heap: Heap)
  type Heap = Map[String, Either[Value, Task]]

  final case class Sequent(
      equations: List[(NF, NF)],
      antes: List[Sequent],
      values: List[Value]
  )

  //switching to eager evaluation
  def deepAnalysis(sentence: Sentence): Sequent = {
    @tailrec def helper(
        in: List[A],
        out: List[(Sentence, List[(NF, NF)], List[A], List[Value])],
        offset: Int
    ): Sequent = in match {
      case Nil =>
        out.foldLeft(Map.empty[Sentence, Sequent]) {
          case (o, (a, b, c, d)) =>
            o + (a -> Sequent(b, c.map(_.sentence).map(o), d))
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
  ): (List[(NF, NF)], List[A], List[Value], Int) = sentence match {
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

      val (lValue, lOffset) = fullEval(Task(left, heap), offset)
      val (rValue, rOffset) =
        fullEval(
          Task(right, heap),
          lOffset
        )
      (equate((lValue, rValue) :: Nil), antes, values, rOffset)
  }

  def equate(in: List[(NF, NF)]) = {
    @tailrec def helper(
        in: List[(NF, NF)],
        out: List[(NF, NF)]
    ): List[(NF, NF)] = in match {
      case Nil => out
      case h :: t =>
        h match {
          case (NF(a, b, c), NF(d, e, f)) =>
            val i = c.indexOf(a)
            if (c.length == f.length &&
                b.length == e.length &&
                i == f.indexOf(d) && (i >= 0 || a == d))
              helper(
                b.map(x => x.copy(args = x.args ++ c))
                  .zip(e.map(x => x.copy(args = x.args ++ f))) ++ t,
                out
              )
            else
              helper(t, h :: out)
        }
    }

    helper(in, Nil)
  }

  final case class NF(operand: Value, operands: List[NF], args: List[Value])

  def fullEval(
      task: Task,
      offset: Int
  ): (NF, Int) = {
    @tailrec def helper(
        in: List[Task],
        out: List[(Task, Value, List[Either[Value, Task]], List[Value])],
        offset: Int
    ): (NF, Int) =
      in match {
        case Nil =>
          val nf = out.foldLeft(Map.empty[Task, NF]) {
            case (o, (a, b, c, d)) =>
              o + (a -> NF(b, c.map {
                case Left(e)  => NF(e, Nil, Nil)
                case Right(t) => o(t)
              }, d))
          }(task)
          (nf, offset)
        case Task(x, y) :: t =>
          val (a, b, c, d) = eval(x, y, Nil, Nil, offset)
          helper(
            b.collect { case Right(x) => x } ++ t,
            (Task(x, y), a, b, c) :: out,
            d
          )
      }
    val (a, b, c, d) = eval(task.term, task.heap, Nil, Nil, offset)
    helper(b.collect { case Right(x) => x }, (task, a, b, c) :: Nil, d)
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
