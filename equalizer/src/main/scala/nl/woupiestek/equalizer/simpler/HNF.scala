package nl.woupiestek.equalizer.simpler

import nl.woupiestek.equalizer.While
import nl.woupiestek.equalizer.While._
import nl.woupiestek.equalizer.simpler.BreakDown._
import scalaz.Scalaz._

final case class HNF(
  arity: Int,
  eqs: List[(HNF, HNF)],
  operator: Either[String, Int],
  operands: List[HNF]) {
  def snf: SNF = SNF(arity, operator, operands)
}

final case class SNF(
  arity: Int,
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
      case Some(Left(c)) => suspend(c.run(arity, eqs, stack))
      case Some(Right(c)) => complete(arity, eqs, Right(c), stack)
    }
    case Abs(a, b) => stack match {
      case Nil => normalize(arity + 1, eqs, b, heap + (a -> Right(arity)), stack)
      case c :: d => normalize(arity, eqs, b, heap + (a -> Left(c)), d)
    }
    case App(a, b) => normalize(arity, eqs, a, heap, task(b, heap) :: stack)
    case Let(a, b, c) =>
      normalize(arity, eqs, c, heap + (a -> Left(task(b, heap))), stack)
    case Check(a, b, c) =>
      normalize(arity, (task(a, heap), task(b, heap)) :: eqs, c, heap, stack)
  }

  final case class Clause(left: SNF, right: SNF, args: List[Clause])

  def clauses(a: HNF, b: HNF): While[List[Clause]] = for {
    c <- a.eqs.traverse { case (x, y) => suspend(clauses(x, y)) }.map(_.flatten)
    d <- b.eqs.traverse { case (x, y) => suspend(clauses(x, y)) }.map(_.flatten)
  } yield Clause(a.snf, b.snf, c ++ d) ::
    c.map(e => e.copy(args = e.args ++ d)) ++
      d.map(f => f.copy(args = f.args ++ c))

}
