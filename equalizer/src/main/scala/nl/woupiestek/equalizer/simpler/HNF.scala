package nl.woupiestek.equalizer.simpler

import nl.woupiestek.equalizer.While
import nl.woupiestek.equalizer.While._
import nl.woupiestek.equalizer.simpler.BreakDown._
import scalaz.Scalaz._

final case class HNF(
  arity: Int,
  eqs: List[(HNF, HNF)],
  operator: Either[String, Int],
  operands: List[HNF])

object HNF {

  final case class Task(run: (Int, List[(Task, Task)], List[Task]) => While[HNF])

  def task(pivot: BreakDown, heap: Map[String, Either[Task, Int]]): Task =
    Task(normalize(_, _, pivot, heap, _))

  def complete(
    arity: Int,
    eqs: List[(Task, Task)],
    operator: Either[String, Int],
    stack: List[Task]): While[HNF] = for {
    a <- eqs.traverse {
      case (b, c) => (b.run(arity, Nil, Nil) |@| c.run(arity, Nil, Nil)).tupled
    }
    b <- stack.traverse(_.run(arity, Nil, Nil))
  } yield HNF(arity, a, operator, b)

  def normalize(
    arity: Int,
    eqs: List[(Task, Task)],
    pivot: BreakDown,
    heap: Map[String, Either[Task, Int]],
    stack: List[Task]): While[HNF] = pivot match {
    case Id(a) => heap.get(a) match {
      case None => complete(arity, eqs, Left(a), stack)
      case Some(Left(c)) => tailCall(c.run(arity, eqs, stack))
      case Some(Right(c)) => complete(arity, eqs, Right(c), stack)
    }
    case Abs(a, b) => stack match {
      case Nil => normalize(arity + 1, eqs, b, heap + (a -> Right(arity)), stack)
      case c :: d => normalize(arity + 1, eqs, b, heap + (a -> Left(c)), d)
    }
    case App(a, b) => normalize(arity, eqs, a, heap, task(b, heap) :: stack)
    case Let(a, b, c) =>
      normalize(arity, eqs, c, heap + (a -> Left(task(b, heap))), stack)
    case Check(a, b, c) =>
      normalize(arity, (task(a, heap), task(b, heap)) :: eqs, c, heap, stack)
  }

  case class SF(arity: Int, operand: Either[String, Int], operator: List[Task])

  case class Prop(left: SF, right: SF, args: List[While[Prop]], arity: Int)

}
