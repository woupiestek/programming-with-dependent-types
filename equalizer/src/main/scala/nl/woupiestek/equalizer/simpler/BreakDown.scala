package nl.woupiestek.equalizer.simpler

sealed trait BreakDown

object BreakDown {

  case class Id(id: String) extends BreakDown

  case class Abs(id: String, body: BreakDown) extends BreakDown

  case class App(operator: BreakDown, operand: BreakDown) extends BreakDown

  case class Let(id: String, value: BreakDown, context: BreakDown) extends BreakDown

  case class Check(left: BreakDown, right: BreakDown, context: BreakDown) extends BreakDown


  //create a set of arrow outside in
  //this requires carrying bound variables up
  //and treating positions carefully
  def arrows(
    entries: List[(Int, Set[(Int, String)], BreakDown)],
    eqs: Set[(Int, Int)],
    ars: Set[(Int, Int, Int)],
    next: Int = 0): (Set[(Int, Int)], Set[(Int, Int, Int)]) =
    entries match {
      case Nil => (eqs, ars)
      case h :: t => h match {
        case (p, v, Id(id)) =>
          val w = v.collect { case (i, j) if j == id => (i, p) }
          arrows(t, eqs ++ w, ars, next)
        case (p, v, Abs(a, b)) =>
          val w = (next + 1, v + ((next, a)), b) :: t
          arrows(w, eqs, ars + ((p, next, next + 1)), next + 2)
        case (p, v, App(a, b)) =>
          val w = (next, v, a) :: (next + 1, v, b) :: t
          arrows(w, eqs, ars + ((next, next + 1, p)), next + 2)
        case (p, v, Let(i, a, b)) =>
          val es = (next, v, a) :: (next + 1, v + ((next + 2, i)), b) :: t
          arrows(es, eqs + ((p, next + 2)) + ((next, next + 1)), ars, next + 3)
        case (p, v, Check(a, b, c)) =>
          val es = (next, v, a) :: (next + 1, v, b) :: (next + 2, v, c) :: t
          arrows(es, eqs + ((next, next + 1)) + ((p, next + 2)), ars, next + 3)
      }
    }

  type NF = (Either[String, Int], List[Task], List[(Task, Task)], Int)

  case class Task(head: BreakDown, tail: Map[String, Either[Task, Int]])

  def normalize(
    pivot: BreakDown,
    heap: Map[String, Either[Task, Int]],
    stack: List[Task] = Nil,
    eqs: List[(Task, Task)] = Nil,
    arity: Int = 0): NF = pivot match {
    case Id(string) => heap.get(string) match {
      case Some(Left(Task(a, b))) => normalize(a, heap ++ b, stack, eqs, arity)
      case Some(Right(a)) => (Right(arity), stack, eqs, arity)
      case None => (Left(string), stack, eqs, arity)
    }
    case Abs(a, b) => stack match {
      case Nil => normalize(b, heap + (a -> Right(arity)), Nil, eqs, arity + 1)
      case h :: t => normalize(b, heap + (a -> Left(h)), t, eqs, arity)
    }
    case App(a, b) => normalize(a, heap, Task(b, heap) :: stack, eqs, arity)
    case Let(i, a, b) =>
      normalize(b, heap + (i -> Left(Task(a, heap))), stack, eqs, arity)
    case Check(a, b, c) =>
      normalize(c, heap, stack, (Task(a, heap), Task(b, heap)) :: eqs, arity)
  }

  def patternMatch: NF => NF = {
    case (operator, operands, equations, arity) =>
      operator match {
        case Left(name) => (operator, operands, equations, arity) //stop right here!
        case Right(index) =>
          val normalized = equations.map { case (a, b) => (
            normalize(a.head, a.tail, arity = arity),
            normalize(b.head, b.tail, arity = arity))
          }
          val select = (normalized ++ normalized.map { case (a, b) => (b, a) })
            .collect {
              case ((a0, a1, a2, a3), (b0, b1, b2, b3)) if a0 == operator =>
                ((a0, a1, a2, a3), (b0, b1, b2, b3))
            }
          ???
      }
  }

  def f(nf0: NF, nf1: NF): ((((Either[String, Int], List[Task], Int), (Either[String, Int], List[Task], Int)), List[(Task, Task)], Int), List[(List[(Task, Task)], (Task, Task))]) = {
    val (a0, a1, a2, a3) = nf0
    val (b0, b1, b2, b3) = nf1
    //the easy set of other equations to hold
    val implications = a2.map((b2, _)) ++ b2.map((a2, _))
    val imp = a3 - b3 match {
      case c if c > 0 => (((a0, a1, 0), (b0, b1, c)), a2, a3)
      case 0 => (((a0, a1, 0), (b0, b1, 0)), a2, a3)
      case c if c < 0 => (((a0, a1, -c), (b0, b1, 0)), b2, b3)
    }
    //consider all the options here
    //the art always revolved around eliminating variable
    //perhaps working systematically
    //perhaps starting earlier...

    //eliminate the head variable if possible,

    (imp, implications)
  }

}
