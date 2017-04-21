package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType.{ Var, arrow, forall, intersection }
import nl.woupiestek.andrej.intersections.Prop.combine
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

object Tagless2 {
  case class State(bounds: Set[(Int, InType)], arity: Int)

  type Term = List[InType] => Stateful[State, InType]

  val pop: Stateful[State, InType] = Stateful(s => (intersection(s.bounds.collect { case (0, t) => t }),
    s.copy(bounds = s.bounds.collect { case (j, t) if j > 0 => (j - 1, t) })))

  val fresh: Stateful[State, InType] = Stateful(s => (Var(s.arity), s.copy(arity = 1 + s.arity)))

  def bind(i: Int, t: InType): Stateful[State, Unit] = Stateful(s => ((), s.copy(bounds = s.bounds + ((i, t)))))

  implicit val instance: UntypedLambdaTerm[Term] = new UntypedLambdaTerm[Term] {
    override def variable(index: Int): Term = args => for {
      v <- fresh
      _ <- bind(index, arrow(args, v))
    } yield v

    override def application(operator: Term, operand: Term): Term = args => for {
      tx <- operand(Nil)
      ty <- operator(tx :: args)
    } yield ty

    override def abstraction(term: Term): Term = args => for {
      tt <- term(args)
      t0 <- pop
    } yield combine(arrow(t0, tt), args)
  }

  def typeOf(term: Term): InType = term(Nil).go(State(Set.empty, 0)) match {
    case (typ, state) => typ
  }
}
