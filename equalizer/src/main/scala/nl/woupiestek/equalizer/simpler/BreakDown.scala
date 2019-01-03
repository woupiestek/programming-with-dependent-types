package nl.woupiestek.equalizer.simpler

sealed abstract class BreakDown

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

  case class Task(head: BreakDown, tail: Map[String, Either[Task, Int]]) {
    def normalized(arity: Int = 0): NF = normalize(head, tail, arity = arity)
  }

  case class NF(
    operator: Either[String, Int],
    operands: List[Task],
    domain: List[(Task, Task)],
    arity: Int)

  def normalize(
    pivot: BreakDown,
    heap: Map[String, Either[Task, Int]],
    stack: List[Task] = Nil,
    eqs: List[(Task, Task)] = Nil,
    arity: Int = 0): NF = pivot match {
    case Id(string) => heap.get(string) match {
      case Some(Left(Task(a, b))) => normalize(a, heap ++ b, stack, eqs, arity)
      case Some(Right(a)) => NF(Right(a), stack, eqs, arity)
      case None => NF(Left(string), stack, eqs, arity)
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

  case class SF(
    operator: Either[String, Int],
    operands: List[Task],
    arity: Int)

  case class Clause[X, Y](left: X, right: X, args: List[Y], index: Int)

  def simplify(ab: Clause[NF, (NF, NF)]): (Clause[SF, (NF, NF)], List[Clause[NF, (NF, NF)]]) = {
    val Clause(NF(a0, a1, a2, a3), NF(b0, b1, b2, b3), e, max) = ab

    def norms: ((Task, Task)) => (NF, NF) = {
      case (x, y) => (x.normalized(max), y.normalized(max))
    }

    val c = a2.map(norms)
    val d = b2.map(norms)
    (Clause(SF(a0, a1, a3), SF(b0, b1, b3), c ++ d ++ e, max),
      c.map { case (c0, c1) => Clause(c0, c1, d ++ e, max) } ++
        d.map { case (d0, d1) => Clause(d0, d1, c ++ e, max) })
  }

  def simplify3(ab: List[Clause[NF, (NF, NF)]], cd: List[Clause[SF, (NF, NF)]]): List[Clause[SF, (NF, NF)]] =
    ab match {
      case Nil => cd
      case h :: t =>
        val (c, a) = simplify(h)
        simplify3(a ++ t, c :: cd)
    }

  def simplify4(eqs: List[(NF, NF)], arity: Int): List[Clause[SF, (NF, NF)]] =
    simplify3(
      eqs.map { case (x, y) => Clause[NF, (NF, NF)](x, y, Nil, arity) },
      Nil)

}
