package nl.woupiestek.equalizer.simpler

import scalaz._
import Scalaz._
import Free._

sealed abstract class BreakDown

object BreakDown {
  case class Iden(id: String) extends BreakDown

  case class Abs(id: String, body: BreakDown) extends BreakDown

  case class App(operator: BreakDown, operand: BreakDown)
      extends BreakDown

  case class Let(
      id: String,
      value: BreakDown,
      context: BreakDown
  ) extends BreakDown

  case class Check(
      left: BreakDown,
      right: BreakDown,
      context: BreakDown
  ) extends BreakDown

  //create a set of arrow outside in
  //this requires carrying bound variables up
  //and treating positions carefully
  def arrows(
      entries: List[(Int, Set[(Int, String)], BreakDown)],
      eqs: Set[(Int, Int)],
      ars: Set[(Int, Int, Int)],
      next: Int = 0
  ): (Set[(Int, Int)], Set[(Int, Int, Int)]) =
    entries match {
      case Nil => (eqs, ars)
      case h :: t =>
        h match {
          case (p, v, Iden(k)) =>
            val w = v.collect {
              case (i, j) if j == k => (i, p)
            }
            arrows(t, eqs ++ w, ars, next)
          case (p, v, Abs(a, b)) =>
            val w = (next + 1, v + ((next, a)), b) :: t
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
          case (p, v, Let(i, a, b)) =>
            val es = (next, v, a) :: (
              next + 1,
              v + ((next + 2, i)),
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

  case class Task(
      head: BreakDown,
      tail: Map[String, Either[Task, Int]]
  ) {
    def normalized(arity: Int = 0): HNF[Task] =
      normalize(head, tail, arity = arity)
  }

  case class HNF[T](
      operator: Either[String, Int],
      operands: List[T],
      domain: List[(T, T)],
      arity: Int
  )

  def normalize(
      pivot: BreakDown,
      heap: Map[String, Either[Task, Int]],
      stack: List[Task] = Nil,
      eqs: List[(Task, Task)] = Nil,
      arity: Int = 0
  ): HNF[Task] = pivot match {
    case Iden(string) =>
      heap.get(string) match {
        case Some(Left(Task(a, b))) =>
          normalize(a, heap ++ b, stack, eqs, arity)
        case Some(Right(a)) => HNF(Right(a), stack, eqs, arity)
        case None           => HNF(Left(string), stack, eqs, arity)
      }
    case Abs(a, b) =>
      stack match {
        case Nil =>
          normalize(
            b,
            heap + (a -> Right(arity)),
            Nil,
            eqs,
            arity + 1
          )
        case h :: t =>
          normalize(b, heap + (a -> Left(h)), t, eqs, arity)
      }
    case App(a, b) =>
      normalize(a, heap, Task(b, heap) :: stack, eqs, arity)
    case Let(i, a, b) =>
      normalize(
        b,
        heap + (i -> Left(Task(a, heap))),
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

  //apply & substitute
  final case class NF(
      operator: Either[String, Int],
      operands: List[NF],
      domain: List[(NF, NF)],
      arity: Int
  )

  final case class Task2(
      run: (List[Task2], List[(NF, NF)], Int) => Trampoline[NF]
  )

  def normalize2(
      pivot: BreakDown,
      heap: Map[String, Either[Task2, Int]],
      stack: List[Task2],
      eqs: List[(NF, NF)],
      arity: Int
  ): Trampoline[NF] = pivot match {
    case Iden(string) =>
      heap.get(string) match {
        case Some(Left(a)) => suspend(a.run(stack, eqs, arity))
        case Some(Right(a)) =>
          stack
            .traverse(_.run(Nil, Nil, arity))
            .map(NF(Right(a), _, eqs, arity))
        case None =>
          stack
            .traverse(_.run(Nil, Nil, arity))
            .map(NF(Left(string), _, eqs, arity))
      }
    case Abs(a, b) =>
      stack match {
        case Nil =>
          normalize2(
            b,
            heap + (a -> Right(arity)),
            Nil,
            eqs,
            arity + 1
          )
        case h :: t =>
          normalize2(b, heap + (a -> Left(h)), t, eqs, arity)
      }
    case App(a, b) =>
      normalize2(
        a,
        heap,
        Task2(normalize2(b, heap, _, _, _)) :: stack,
        eqs,
        arity
      )
    case Let(i, a, b) =>
      normalize2(
        b,
        heap + (i -> Left(Task2(normalize2(a, heap, _, _, _)))),
        stack,
        eqs,
        arity
      )
    case Check(a, b, c) =>
      for {
        d <- normalize2(a, heap, Nil, Nil, arity)
        e <- normalize2(b, heap, Nil, Nil, arity)
        f <- normalize2(c, heap, stack, (d, e) :: eqs, arity)
      } yield f
  }

  case class SF(
      operator: Either[String, Int],
      operands: List[Task],
      arity: Int
  )

  case class Clause[X, Y](
      left: X,
      right: X,
      args: List[Y],
      index: Int
  )

  def simplify(ab: Clause[HNF[Task], (HNF[Task], HNF[Task])]): (
      Clause[SF, (HNF[Task], HNF[Task])],
      List[Clause[HNF[Task], (HNF[Task], HNF[Task])]]
  ) = {
    val Clause(
      HNF(a0, a1, a2, a3),
      HNF(b0, b1, b2, b3),
      e,
      max
    ) = ab

    def norms: ((Task, Task)) => (HNF[Task], HNF[Task]) = {
      case (x, y) => (x.normalized(max), y.normalized(max))
    }

    val c = a2.map(norms)
    val d = b2.map(norms)
    (
      Clause(SF(a0, a1, a3), SF(b0, b1, b3), c ++ d ++ e, max),
      c.map { case (c0, c1)   => Clause(c0, c1, d ++ e, max) } ++
        d.map { case (d0, d1) => Clause(d0, d1, c ++ e, max) }
    )
  }

  def simplify3(
      ab: List[Clause[HNF[Task], (HNF[Task], HNF[Task])]],
      cd: List[Clause[SF, (HNF[Task], HNF[Task])]]
  ): List[Clause[SF, (HNF[Task], HNF[Task])]] =
    ab match {
      case Nil => cd
      case h :: t =>
        val (c, a) = simplify(h)
        simplify3(a ++ t, c :: cd)
    }

  def simplify4(
      eqs: List[(HNF[Task], HNF[Task])],
      arity: Int
  ): List[Clause[SF, (HNF[Task], HNF[Task])]] =
    simplify3(eqs.map {
      case (x, y) =>
        Clause[HNF[Task], (HNF[Task], HNF[Task])](
          x,
          y,
          Nil,
          arity
        )
    }, Nil)

}
