package nl.woupiestek.equalizer.simpler

import nl.woupiestek.equalizer.While
import nl.woupiestek.equalizer.While._
import nl.woupiestek.equalizer.simpler.BreakDown._
import scalaz.Scalaz._

import scala.annotation.tailrec

final case class HNF(
  arity: Int,
  eqs: List[CNF],
  operator: Either[String, Int],
  operands: List[HNF]) {
  def snf: SNF = SNF(arity, operator, operands)
}

final case class SNF(
  arity: Int,
  operator: Either[String, Int],
  operands: List[HNF])

final case class CNF(left: SNF, right: SNF, args: List[CNF])

object HNF {
  @tailrec def normalize(
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

  def task(pivot: BreakDown, heap: Map[String, Either[Task, Int]]) =
    Task(normalize(_, _, pivot, heap, _))

  final case class Task(run: (Int, List[(Task, Task)], List[Task]) => While[HNF]) {
    def value(arity: Int): While[HNF] = run(0, Nil, Nil)
  }

  type K = Map[String, Either[Task, Int]] => Task

  val instance: TermLike[String, K] = new TermLike[String, K] {
    override def variable(i: String): K = h => Task((a, e, s) =>
      h.get(i) match {
        case None => complete(a, e, Left(i), s)
        case Some(Left(c)) => suspend(c.run(a, e, s))
        case Some(Right(c)) => complete(a, e, Right(c), s)
      })

    override def lambda(i: String, b: K): K = h => Task((a, e, s) => s match {
      case Nil => b(h + (i -> Right(a))).run(a + 1, e, s)
      case c :: d => b(h + (i -> Left(c))).run(a, e, d)
    })

    override def apply(x: K, y: K): K = h =>
      Task((a, e, s) => x(h).run(a, e, y(h) :: s))

    override def let(i: String, v: K, c: K): K = h => c(h + (i -> Left(v(h))))

    override def check(l: K, r: K, c: K): K = h =>
      Task((a, e, s) => c(h).run(a, (l(h), r(h)) :: e, s))
  }

  private def complete(
    arity: Int,
    eqs: List[(Task, Task)],
    operator: Either[String, Int],
    stack: List[Task]): While[HNF] =
    (eqs.traverse {
      case (b, c) => (b.value(arity) |@| c.value(arity)) ((a, b) =>
        CNF(a.snf, b.snf, a.eqs ++ b.eqs) ::
          a.eqs.map(e => e.copy(args = e.args ++ b.eqs)) ++
            b.eqs.map(f => f.copy(args = f.args ++ a.eqs)))
    }.map(_.flatten) |@|
      stack.traverse(_.value(arity))) (HNF(arity, _, operator, _))
}
