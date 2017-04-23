package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType._
import nl.woupiestek.andrej.intersections.Stateful.point

sealed trait InTerm

object InTerm {

  case class Variable(index: Int) extends InTerm

  case class Application(operator: InTerm, operand: InTerm) extends InTerm

  case class Abstraction(inTerm: InTerm) extends InTerm

  case class State(bounds: Set[(Int, InType)], arity: Int) {
    def withBound(i: Int, inType: InType): State = copy(bounds = bounds + ((i, inType)))

    def incr: State = copy(arity = arity + 1)
  }

  val start = State(Set.empty, 0)

  def pop: Stateful[State, InType] = Stateful { s =>
    (intersection(s.bounds.collect { case (0, b) => b }),
      s.copy(bounds = s.bounds.collect { case (j, t) if j > 0 => (j - 1, t) }))
  }

  def typed(term: InTerm, context: List[InType], args: List[InType]): Stateful[State, InType] = term match {
    case Variable(i) => context lift i match {
      case Some(t) => point(ponens(t, args))
      case None => Stateful(s => (Var(s.arity), s.incr.withBound(i, arrow(args, Var(s.arity)))))
    }
    case Application(m, n) => for {
      tn <- typed(n, context, Nil)
      tm <- typed(m, context, tn :: args)
    } yield tm
    case Abstraction(t) => args match {
      case head :: tail => typed(t, head :: context, tail)
      case Nil => for {
        tt <- typed(t, context, Nil)
        t0 <- pop
      } yield arrow(t0, tt)
    }
  }

  def typeOf(term: InTerm): InType = InTerm.typed(term, Nil, Nil).go(start)._1

}

