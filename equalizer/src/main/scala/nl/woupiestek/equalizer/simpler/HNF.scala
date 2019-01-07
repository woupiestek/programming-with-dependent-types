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
    case App(a, b) => normalize(arity, eqs, a, heap, Task(b, heap) :: stack)
    case Let(a, b, c) =>
      normalize(arity, eqs, c, heap + (a -> Left(Task(b, heap))), stack)
    case Check(a, b, c) =>
      normalize(arity, (Task(a, heap), Task(b, heap)) :: eqs, c, heap, stack)
  }

  private final case class Task(pivot: BreakDown, heap: Map[String, Either[Task, Int]]) {
    def run(arity: Int, eqs: List[(Task, Task)], stack: List[Task]): While[HNF] =
      normalize(arity, eqs, pivot, heap, stack)

    def value(arity: Int): While[HNF] = run(arity, Nil, Nil)
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


  final case class J(run: (Int, List[(J, J)], List[J]) => While[HNF]) {
    def value(arity: Int): While[HNF] = run(0, Nil, Nil)
  }

  type K = Map[String, Either[J, Int]] => J

  val instance: TermLike[String, K] = new TermLike[String, K] {
    override def variable(id: String): K = heap => J((arity, eqs, stack) =>
      heap.get(id) match {
        case None => complete2(arity, eqs, Left(id), stack)
        case Some(Left(c)) => suspend(c.run(arity, eqs, stack))
        case Some(Right(c)) => complete2(arity, eqs, Right(c), stack)
      })

    override def lambda(id: String, body: K): K = heap => J((arity, eqs, stack) =>
      stack match {
        case Nil => body(heap + (id -> Right(arity))).run(arity + 1, eqs, stack)
        case c :: d => body(heap + (id -> Left(c))).run(arity, eqs, d)
      })

    override def apply(operator: K, operand: K): K = heap => J((arity, eqs, stack) =>
      operator(heap).run(arity, eqs, operand(heap) :: stack))

    override def let(id: String, value: K, context: K): K = heap => J((arity, eqs, stack) =>
      context(heap + (id -> Left(value(heap)))).run(arity, eqs, stack))

    override def check(left: K, right: K, context: K): K = heap => J((arity, eqs, stack) =>
      context(heap).run(arity, (left(heap), right(heap)) :: eqs, stack))
  }

  private def complete2(
    arity: Int,
    eqs: List[(J, J)],
    operator: Either[String, Int],
    stack: List[J]): While[HNF] =
    (eqs.traverse {
      case (b, c) => (b.value(arity) |@| c.value(arity)) ((a, b) =>
        CNF(a.snf, b.snf, a.eqs ++ b.eqs) ::
          a.eqs.map(e => e.copy(args = e.args ++ b.eqs)) ++
            b.eqs.map(f => f.copy(args = f.args ++ a.eqs)))
    }.map(_.flatten) |@|
      stack.traverse(_.value(arity))) (HNF(arity, _, operator, _))
}
