package nl.woupiestek.equalizer.simpler

sealed abstract class BreakDown2

object BreakDown2 {

  case class Index(value: Int) extends BreakDown2

  case class Abs(body: BreakDown2) extends BreakDown2

  case class App(operator: BreakDown2, operand: BreakDown2)
      extends BreakDown2

  case class Push(value: BreakDown2, context: BreakDown2)
      extends BreakDown2

  case class Check(
      left: BreakDown2,
      right: BreakDown2,
      context: BreakDown2
  ) extends BreakDown2

  //create a set of arrow outside in
  //this requires carrying bound variables up
  //and treating positions carefully
  def arrows(
      entries: List[(Int, List[Int], BreakDown2)],
      eqs: Set[(Int, Int)],
      ars: Set[(Int, Int, Int)],
      next: Int = 0
  ): (Set[(Int, Int)], Set[(Int, Int, Int)]) =
    entries match {
      case Nil => (eqs, ars)
      case h :: t =>
        h match {
          case (p, v, Index(id)) =>
            val w = v.lift(id).map((_, p))
            arrows(t, eqs ++ w, ars, next)
          case (p, v, Abs(b)) =>
            val w = (next + 1, next :: v, b) :: t
            arrows(
              w,
              eqs,
              ars + ((p, next, next + 1)),
              next + 2
            )
          case (p, v, App(a, b)) =>
            val w = (next, v, a) :: (next + 1, v, b) :: t
            arrows(
              w,
              eqs,
              ars + ((next, next + 1, p)),
              next + 2
            )
          case (p, v, Push(a, b)) =>
            val es = (next, v, a) :: (
              next + 1,
              (next + 2) :: v,
              b
            ) :: t
            arrows(
              es,
              eqs + ((p, next + 2)) + ((next, next + 1)),
              ars,
              next + 3
            )
          case (p, v, Check(a, b, c)) =>
            val es = (next, v, a) :: (next + 1, v, b) :: (
              next + 2,
              v,
              c
            ) :: t
            arrows(
              es,
              eqs + ((next, next + 1)) + ((p, next + 2)),
              ars,
              next + 3
            )
        }
    }

  type NF =
    (Either[Int, Int], List[Task], List[(Task, Task)], Int)

  case class Task(
      head: BreakDown2,
      tail: List[Either[Task, Int]]
  ) {
    def normalized: NF = normalize(head, tail)
  }

  def normalize(
      pivot: BreakDown2,
      heap: List[Either[Task, Int]],
      stack: List[Task] = Nil,
      eqs: List[(Task, Task)] = Nil,
      arity: Int = 0
  ): NF = pivot match {
    case Index(string) =>
      heap.lift(string) match {
        case Some(Left(Task(a, b))) =>
          normalize(a, b ++ heap, stack, eqs, arity)
        case Some(Right(index)) =>
          (Right(index), stack, eqs, arity) //don't know what to do with these
        case None => (Left(string), stack, eqs, arity)
      }
    case Abs(b) =>
      stack match {
        case Nil =>
          normalize(
            b,
            Right(arity) :: heap,
            Nil,
            eqs,
            arity + 1
          ) //moving inside bindings is dangerous...
        case h :: t =>
          normalize(b, Left(h) :: heap, t, eqs, arity)
      }
    case App(a, b) =>
      normalize(a, heap, Task(b, heap) :: stack, eqs, arity)
    case Push(a, b) =>
      normalize(
        b,
        Left(Task(a, heap)) :: heap,
        stack,
        eqs,
        arity
      )
    case Check(a, b, c) =>
      normalize(
        c,
        heap,
        stack,
        (Task(a, heap), Task(b, heap)) :: eqs,
        arity
      )
  }

  //
}
