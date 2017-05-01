package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._
import nl.woupiestek.andrej.intersections.Stateful.point
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

object Tagless {

  case class State(bounds: Set[(Int, RType)], arity: Int)

  type Term = (List[LType], List[LType]) => Stateful[State, LType]

  val pop: Stateful[State, LType] = Stateful(s => (LType(s.bounds.collect {
    case (0, t) => t
  }), s.copy(bounds = s.bounds.collect { case (j, t) if j > 0 => (j - 1, t) })))

  val fresh: Stateful[State, LType] = Stateful(s => (parameter(s.arity), s.copy(arity = 1 + s.arity)))

  def bind(i: Int, t: LType): Stateful[State, Unit] = Stateful(s => ((), s.copy(bounds = s.bounds ++ t.rTypes.map((i, _)))))

  implicit val instance: UntypedLambdaTerm[Term] = new UntypedLambdaTerm[Term] {

    override def variable(index: Int): Term = (context, args) => context.lift(index) match {
      case Some(t) => point(Combinator.combine(t, args))
      case None => for {
        v <- fresh
        _ <- bind(index, args.foldRight(v)(arrow))
      } yield v
    }

    override def application(operator: Term, operand: Term): Term = (context, args) => for {
      tx <- operand(context, Nil)
      ty <- operator(context, tx :: args)
    } yield ty

    override def abstraction(term: Term): Term = (context, args) => /*for {
      tt <- term(context, Nil)
      t0 <- pop
    } yield args.foldLeft(arrow(t0, tt))(Combinator2.combine) */ //Combinator.combine(arrow(t0, tt), args)
      args match {
        case Nil => for {
          tt <- term(context, Nil)
          t0 <- pop
        } yield arrow(t0, tt) //hier zouden de nieuwe variabelen gebonden kunnen worden.
        case head :: tail => term(head :: context, tail)
      }
  }

  def typeOf(term: Term): LType = term(Nil, Nil).go(State(Set.empty, 0))._1
}

