package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer3 {

  final case class Value(name: String, level: Int)
  final case class A(sentence: Sentence, args: List[Value])
  final case class Task(term: Term, heap: Heap)
  type Heap = Map[String, Either[Value, Task]]

  final case class NF(operator: Value, operands: List[NF], args: List[Value]) {
    def contour: (Either[Value, Int], Int, Int) = {
      val index = args.indexOf(operator)
      (
        if (index >= 0) Right(index) else Left(operator),
        operands.length,
        args.length
      )
    }
    def subforms = {
      operands.map(nf => nf.copy(args = nf.args ++ args))
    }
  }

  final case class Sequent(
      left: NF,
      right: NF,
      antes: Set[Sequent],
      values: Set[Value]
  ) {
    def vertices =
      Set(
        (left.operator, left.operands.length),
        (right.operator, right.operands.length)
      ).filter {
        case (v, _) => values(v)
      }

    def chew = {
      val vs: Set[Value] = values ++ left.args ++ right.args
      (left.args.length - right.args.length) match {
        case x if x < 0 =>
          val qs = right.args.drop(-x).zip(left.args)
          val y = right.args.reverse.take(-x).map(Left(_))
          (
            left.operator,
            left.operands ++ y,
            right.operator,
            right.operands,
            antes,
            qs,
            vs
          )
        case 0 =>
          (
            left.operator,
            left.operands,
            right.operator,
            right.operands,
            antes,
            left.args.zip(right.args),
            vs
          )
        case x if x > 0 =>
          val qs = left.args.drop(x).zip(right.args)
          val y = left.args.reverse.take(x).map(Left(_))
          (
            left.operator,
            left.operands,
            right.operator,
            right.operands ++ y,
            antes,
            qs,
            vs
          )

      }
    }
  }

  //matches...
  def judge(
      sequent: Sequent,
      conjunction: List[Sequent],
      disjunction: List[List[Sequent]]
  ): Boolean = {
    if (sequent.left.contour == sequent.right.contour) {
      val x = sequent.left.subforms.zip(sequent.right.subforms).map {
        case (l, r) => Sequent(l, r, sequent.antes, sequent.values)
      }
      x ++ conjunction match {
        case Nil    => true
        case h :: t => judge(h, t, disjunction)
      }
    } else {
      val matches = sequent.antes
        .filter(_.vertices.exists {
          case (v, i) =>
            sequent.vertices.exists {
              case (w, j) => v == w && i <= j
            }
        })
      matches.map { ante =>
        val gamma = sequent.antes - ante
        val delta = ante.antes.map(
          d =>
            d.copy(
              antes = d.antes ++ gamma,
              values = d.values ++ sequent.values
            )
        )

        val nValues: Set[Value] =
          sequent.left.args.toSet ++
            sequent.right.args ++
            ante.left.args ++
            ante.right.args
        val equations: Set[(Value, Value)] =
          sequent.left.args
            .zip(sequent.right.args)
            .toSet ++
            ante.left.args.zip(ante.right.args)

        val (a, b, c, d) = if (sequent.left.operator == ante.left.operator) {
          (sequent.left, ante.left, sequent.right, ante.right)
        } else if (sequent.left.operator == ante.right.operator) {
          (sequent.left, ante.right, sequent.right, ante.left)
        } else if (sequent.right.operator == ante.left.operator) {
          (sequent.right, ante.left, sequent.left, ante.right)
        } else {
          (sequent.right, ante.right, sequent.left, ante.left)
        }

        val equations2: Set[(NF, NF)] = a.operands.zip(b.operands).toSet

        //shit, I know what has to happen
        //but it remain to difficult to do here.
        //also, I am missing a lot of steps...

        ???
      }
      ???
    }
  }

  //switching to eager evaluation
  def deepAnalysis(sentence: Sentence): Sequent = {
    @tailrec def helper(
        in: List[A],
        out: List[(Sentence, NF, NF, Set[A], Set[Value])],
        offset: Int
    ): Sequent = in match {
      case Nil =>
        out.foldLeft(Map.empty[Sentence, Sequent]) {
          case (o, (a, l, r, c, d)) =>
            val f = c.map(_.sentence).map(o)
            o + (a -> Sequent(l, r, f, d))
        }(sentence)
      case A(h0, h1) :: t =>
        val (l, r, as, vs, o2) = analyze(h0, Nil, h1, offset)
        helper(as ++ t, (h0, l, r, as.toSet, vs) :: out, o2)
    }
    helper(A(sentence, Nil) :: Nil, Nil, 0)
  }

  @tailrec def analyze(
      sentence: Sentence,
      antes: List[A],
      values: List[Value],
      offset: Int
  ): (NF, NF, List[A], Set[Value], Int) = sentence match {
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

      val (lValue, lOffset) = fullEval(Task(left, heap), Nil, offset)
      val (rValue, rOffset) =
        fullEval(
          Task(right, heap),
          lValue.args,
          lOffset
        )
      (lValue, rValue, antes, values.toSet, rOffset)
  }

  def fullEval(
      task: Task,
      args: List[Value],
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
    val (a, b, c, d) =
      eval(task.term, task.heap, args.reverse.map(Left(_)), Nil, offset)
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
