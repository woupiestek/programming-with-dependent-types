package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

object Tagless {

  case class State(bounds: Set[(Int, Int)], arity: Int, types: Map[Int, RType])

  type Term = Stateful[State, RType]

  val pop: Stateful[State, LType] = Stateful(s => (LType(s.bounds.collect {
    case (0, t) => t
  }.flatMap(s.types.get)), s.copy(bounds = s.bounds.collect { case (j, t) if j > 0 => (j - 1, t) })))

  val fresh: Stateful[State, Int] = Stateful(s => (s.arity, s.copy(arity = 1 + s.arity)))

  def bind(i: Int, t: Int): Stateful[State, Unit] = Stateful(s => ((), s.copy(bounds = s.bounds + ((i, t)))))

  def typeOf(index: Int): Stateful[State, RType] = Stateful(s => (s.types.getOrElse(index, Atomic(Parameter(index))), s))

  def force(x: RType, y: RType): Stateful[State, Boolean] = Stateful(s => {
    LType.solve((x, y) :: Nil, s.types) match {
      case None => (false, s)
      case Some(ts) => (true, s.copy(types = ts))
    }
  })

  implicit val instance: UntypedLambdaTerm[Term] = new UntypedLambdaTerm[Term] {

    override def variable(index: Int): Term = for {
      u <- fresh
      _ <- bind(index, u)
    } yield Atomic(Parameter(u))

    override def application(operator: Term, operand: Term): Term = for {
      v <- fresh.map(u => Atomic(Parameter(u)))
      tx <- operand
      ty <- operator
      _ <- force(ty, left(ty) ->: v)
    } yield v

    override def abstraction(term: Term): Term = for {
      tt <- term
      t0 <- pop
    } yield t0 ->: tt

  }

  def typeOf(term: Term): LType = left(term.go(State(Set.empty, 0, Map.empty))._1)
}

