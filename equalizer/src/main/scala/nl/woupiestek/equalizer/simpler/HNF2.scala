package nl.woupiestek.equalizer.simpler

import nl.woupiestek.equalizer.While
import nl.woupiestek.equalizer.While._
import scalaz.Scalaz._

final case class HNF2(snf: SNF2, eqs: List[CNF2])

final case class SNF2(arity: Int, operator: Either[String, Int], operands: List[HNF2])

final case class CNF2(left: SNF2, right: SNF2, args: List[CNF2])

object HNF2 {

  type Heap = Map[String, Either[Task, Int]]

  type Stack = List[Task]

  final case class Task(run: (Stack, Int) => While[HNF2])

  final case class Term(run: (Heap, Stack, Int) => While[HNF2])

  val instance: TermLike[String, Term] = new TermLike[String, Term] {
    override def variable(i: String): Term = Term((h, s, a) =>
      h.get(i) match {
        case None => complete(Left(i), s, a)
        case Some(Left(c)) => c.run(s, a)
        case Some(Right(c)) => complete(Right(c), s, a)
      })

    override def lambda(i: String, b: Term): Term = Term((h, s, a) => s match {
      case Nil => b.run(h + (i -> Right(a)), s, a + 1)
      case c :: d => b.run(h + (i -> Left(c)), d, a)
    })

    override def apply(x: Term, y: Term): Term =
      Term((h, s, a) => x.run(h, Task(y.run(h, _, _)) :: s, a))

    override def let(i: String, v: Term, c: Term): Term =
      Term((h, s, a) => c.run(h + (i -> Left(Task(v.run(h, _, _)))), s, a))

    override def check(l: Term, r: Term, c: Term): Term = Term((h, s, a) =>
      (l.run(h, Nil, a) |@| r.run(h, Nil, a) |@| c.run(h, s, a)) ((a, b, hnf) =>
        hnf.copy(eqs = CNF2(a.snf, b.snf, a.eqs ++ b.eqs) ::
          a.eqs.map(e => e.copy(args = e.args ++ b.eqs)) ++
            b.eqs.map(f => f.copy(args = f.args ++ a.eqs)) ++
            hnf.eqs)))
  }

  private def complete(i: Either[String, Int], s: Stack, a: Int): While[HNF2] = s
    .traverse(_.run(Nil, a))
    .map((s: List[HNF2]) => HNF2(SNF2(a, i, s), Nil))
}
