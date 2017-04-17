package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType._
import nl.woupiestek.andrej.intersections.Stateful.point
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

object Tagless {

  case class State(bounds: Set[(Int, InType)], arity: Int)

  type Term = (List[InType], List[InType]) => Stateful[State, InType]

  val pop: Stateful[State, InType] = Stateful(s => (intersection(s.bounds.collect { case (0, t) => t }),
    s.copy(bounds = s.bounds.collect { case (j, t) if j > 0 => (j - 1, t) })))

  val fresh: Stateful[State, InType] = Stateful(s => (Var(s.arity), s.copy(arity = 1 + s.arity)))

  def bind(i: Int, t: InType): Stateful[State, Unit] = Stateful(s => ((), s.copy(bounds = s.bounds + ((i, t)))))

  implicit val instance: UntypedLambdaTerm[Term] = new UntypedLambdaTerm[Term] {
    override def variable(index: Int): Term = (context, args) => context.lift(index) match {
      case Some(t) => point(ponens(t, args))
      case None => for {
        v <- fresh
        _ <- bind(index, arrow(args, v))
      } yield v
    }

    override def application(operator: Term, operand: Term): Term = (context, args) => for {
      tx <- operand(context, Nil)
      ty <- operator(context, tx :: args)
    } yield ty

    override def abstraction(term: Term): Term = (context, args) => args match {
      case Nil => for {
        tt <- term(context, args)
        t0 <- pop
      } yield arrow(t0, tt)
      case head :: tail => term(head :: context, tail)
    }
  }

  def typeOf(term: Term): InType = term(Nil, Nil).go(State(Set.empty, 0)) match {
    case (typ, state) => forall(state.arity, typ)
  }

}
