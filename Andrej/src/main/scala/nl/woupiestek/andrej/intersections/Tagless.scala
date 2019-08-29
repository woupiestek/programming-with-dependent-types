package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.AType.{
  Constant,
  Parameter
}
import nl.woupiestek.andrej.intersections.LType._
import nl.woupiestek.andrej.intersections.Stateful.point
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

object Tagless {

  case class State(
      bounds: Set[(Int, RType)],
      arity: Int,
      types: Map[Int, RType]
  )

  type Term =
    List[Option[RType]] => Stateful[State, Option[RType]]

  val pop: Stateful[State, Set[RType]] = Stateful(
    s =>
      (s.bounds.collect {
        case (0, t) => t
      }, s.copy(bounds = s.bounds.collect {
        case (j, t) if j > 0 => (j - 1, t)
      }))
  )

  val fresh: Stateful[State, Int] = Stateful(
    s => (s.arity, s.copy(arity = 1 + s.arity))
  )

  def bind(i: Int, t: RType): Stateful[State, Unit] =
    Stateful(s => ((), s.copy(bounds = s.bounds + ((i, t)))))

  def typeOf(index: Int): Stateful[State, RType] =
    Stateful(
      s =>
        (s.types.getOrElse(index, Atomic(Parameter(index))), s)
    )

  def eliminate: Stateful[State, Unit] =
    Stateful(s => {
      val nbs = s.bounds.map {
        case (i, t) =>
          (i, s.types.foldLeft(t) {
            case (u, (j, v)) => u.replace(j, v)
          })
      }
      ((), s.copy(bounds = nbs))
    })

  def force(x: RType, y: RType): Stateful[State, Boolean] =
    Stateful(s => {
      LType.solve((x, y) :: Nil, s.types) match {
        case None => (false, s)
        case Some(nts) =>
          val nbs = s.bounds.map {
            case (i, t) =>
              (i, nts.foldLeft(t) {
                case (u, (j, v)) => u.replace(j, v)
              })
          }
          (true, s.copy(bounds = nbs, types = nts))
      }
    })

  implicit val instance: UntypedLambdaTerm[Term] =
    new UntypedLambdaTerm[Term] {

      override def variable(index: Int): Term =
        args =>
          for {
            u <- fresh
            _ <- bind(
              index,
              RType(args.map(left), Parameter(u))
            )
          } yield Some(Atomic(Parameter(u)))

      override def application(
          operator: Term,
          operand: Term
      ): Term =
        args =>
          for {
            tx <- operand(Nil)
            ty <- operator(tx :: args)
          } yield ty

      def foldLeft[A, B](args: List[A])(
          acc: Stateful[State, B]
      )(op: (B, A) => Stateful[State, B]): Stateful[State, B] =
        args match {
          case Nil    => acc
          case h :: t => foldLeft(t)(acc.flatMap(op(_, h)))(op)
        }

      override def abstraction(term: Term): Term = args => {
        for {
          tt <- term(Nil)
          t0 <- pop
          r <- foldLeft(args)(point(tt.map(LType(t0) ->: _)))(
            combine
          )
        } yield r
      }

    }

  def traverse[A, B](t0: List[A])(
      f: A => Stateful[State, B]
  ): Stateful[State, List[B]] = t0 match {
    case Nil => point(Nil)
    case h :: t =>
      for {
        h2 <- f(h)
        t2 <- traverse(t)(f)
      } yield h2 :: t2
  }

  private def combine(
      rType: Option[RType],
      arg: Option[RType]
  ): Stateful[State, Option[RType]] = rType match {
    case Some(a ->: b) =>
      for {
        bools <- traverse(a.rTypes.toList)(
          at =>
            arg match {
              case None     => point(false)
              case Some(bt) => force(at, bt)
            }
        )
        x <- Stateful[State, Option[RType]](
          s =>
            (
              if (bools.forall(b => b))
                Some(s.types.foldLeft(b) {
                  case (u, (j, v)) => u.replace(j, v)
                })
              else None,
              s
            )
        )
      } yield x
    case Some(Atomic(Parameter(i))) =>
      typeOf(i).flatMap(ti => combine(Some(ti), arg))
    case Some(Atomic(Constant(_))) => point(None)
  }

  def typeOf(term: Term): LType = LType(fromZero(term)._1.toSet)

  def fromZero(term: Term): (Option[RType], State) = {
    term(Nil).go(State(Set.empty, 0, Map.empty))
  }
}
