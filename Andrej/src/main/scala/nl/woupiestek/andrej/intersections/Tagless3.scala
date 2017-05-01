package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

class Tagless3 {

  case class State(bounds: Set[(Int, RType)], leqs: Set[(RType, RType)], arity: Int)

  type Term = RType => Stateful[State, RType]

  def order(x: RType, y: RType): Stateful[State, Unit] = Stateful(s => ((), s.copy(leqs = s.leqs + ((x, y)))))

  val pop: Stateful[State, LType] = Stateful(s => (LType(s.bounds.collect {
    case (0, t) => t
  }), s.copy(bounds = s.bounds.collect { case (j, t) if j > 0 => (j - 1, t) })))

  val fresh: Stateful[State, RType] = Stateful(s => (Atomic(Parameter(s.arity)), s.copy(arity = 1 + s.arity)))

  implicit val check: UntypedLambdaTerm[Term] = new UntypedLambdaTerm[Term] {

    override def variable(index: Int): Term = target =>
      Stateful(s => (target, s.copy(bounds = s.bounds + ((index, target)))))

    override def application(operator: Term, operand: Term): Term = target => for {
      v <- fresh
      tx <- operand(v)
      ty <- operator(left(tx) ->: target)
    } yield ty

    override def abstraction(term: Term): Term = target => for {
      v <- fresh
      tt <- term(v)
      t0 <- pop
      _ <- order(target, t0 ->: tt)
    } yield t0 ->: tt //only place where a new assumption is recorded, if I am not mistaken
  }

  //targets acquire arguments and upperbounds. Don't see any lower bounds however.
  //stuck repeatedly at solving systems of inequalities.
  //variables have positive and negative occurrences
  //which make elimination harder, and pontentially loosing information

  //def typeOf(term:Term):LType

}
