package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType.{
  Arrow,
  Parameter,
  replace,
  solve
}
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

object Tagless2 {

  case class State(bounds: Set[(Int, InType)], arity: Int)

  type Term =
    List[Option[InType]] => Stateful[State, Option[InType]]

  val pop: Stateful[State, Set[InType]] = Stateful(
    s =>
      (
        s.bounds.collect { case (0, t) => t },
        s.copy(bounds = s.bounds.collect {
          case (j, t) if j > 0 => (j - 1, t)
        })
      )
  )

  val fresh: Stateful[State, InType] = Stateful(
    s => (Parameter(s.arity), s.copy(arity = 1 + s.arity))
  )

  def bind(i: Int, t: InType): Stateful[State, Unit] =
    Stateful(s => ((), s.copy(bounds = s.bounds + ((i, t)))))

  implicit val instance: UntypedLambdaTerm[Term] =
    new UntypedLambdaTerm[Term] {
      override def variable(index: Int): Term =
        args =>
          for {
            v <- fresh
            _ <- bind(index, args.foldRight(v) {
              case (a, b) => Arrow(a.toSet, b)
            })
          } yield Some(v)

      override def application(
          operator: Term,
          operand: Term
      ): Term =
        args =>
          for {
            tx <- operand(Nil)
            ty <- operator(tx :: args)
          } yield ty

      override def abstraction(term: Term): Term =
        args =>
          for {
            tt <- term(Nil)
            t0 <- pop
          } yield tt.flatMap(t1 => combine(Arrow(t0, t1), args))
    }

  def combine(x: InType, y: Option[InType]): Option[InType] =
    (x, y) match {
      case (Arrow(a, b), Some(c)) =>
        solve(a.toList.map(z => (c, z)), Map.empty)
          .map(_.foldLeft(b) {
            case (d, (i, e)) => replace(e, i)(d)
          })
      case (Arrow(a, b), None) if a.isEmpty => Some(b)
      case _                                => None
    }

  def combine(
      x: InType,
      y: List[Option[InType]]
  ): Option[InType] = y match {
    case Nil => Some(x)
    case h :: t =>
      for {
        a <- combine(x, h)
        b <- combine(a, t)
      } yield b
  }

  def typeOf(term: Term): Option[InType] =
    term(Nil).go(State(Set.empty, 0)) match {
      case (typ, _) => typ
    }
}
