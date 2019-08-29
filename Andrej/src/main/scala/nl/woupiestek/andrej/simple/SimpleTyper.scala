package nl.woupiestek.andrej.simple

import nl.woupiestek.andrej.simple.SimpleType.{%, ->:}
import nl.woupiestek.andrej.typeclasses.{
  Monad,
  UntypedLambdaTerm
}
import Monad._

object SimpleTyper {

  case class M[T](run: Int => (T, Set[(Int, SimpleType)], Int))

  implicit val m_is_a_monad: Monad[M] = new Monad[M] {
    override def unit[A](a: A): M[A] = M(c => (a, Set.empty, c))

    override def bind[A, B](fa: M[A])(f: (A) => M[B]): M[B] =
      M(
        c =>
          fa.run(c) match {
            case (a2, b2, c2) =>
              f(a2).run(c2) match {
                case (a3, b3, c3) => (a3, b2 union b3, c3)
              }
          }
      )
  }

  type Term = (List[SimpleType], SimpleType) => M[Unit]

  private def fresh =
    M(arity => (%(arity), Set.empty, arity + 1))

  private def unify(a: SimpleType, b: SimpleType) =
    M(arity => ((), Solver.analyze(a, b), arity))

  implicit val term_is_lambda: UntypedLambdaTerm[Term] =
    new UntypedLambdaTerm[Term] {
      override def abstraction(term: Term): Term =
        (context, target) =>
          for {
            a <- fresh
            b <- fresh
            _ <- term(a :: context, b)
            _ <- unify(target, a ->: b)
          } yield ()

      override def application(
          operator: Term,
          operand: Term
      ): Term =
        (context, target) =>
          for {
            s <- fresh
            _ <- operand(context, s)
            _ <- operator(context, s ->: target)
          } yield ()

      override def variable(index: Int): Term =
        (context, target) =>
          context lift index match {
            case None    => m_is_a_monad.unit(())
            case Some(t) => unify(t, target)
          }
    }

  def typeOf(term: Term): Option[Set[SimpleType]] =
    Solver
      .System(term(Nil, %(0)).run(1)._2)
      .equated
      .solve
      .map(_.eqs.collect { case (0, t) => t })

}

object Solver {

  def analyze
      : (SimpleType, SimpleType) => Set[(Int, SimpleType)] = {
    case (c ->: d, e ->: f)     => analyze(c, e) ++ analyze(d, f)
    case (%(i), %(j)) if i == j => Set.empty
    case (%(i), %(j)) if i > j  => Set((j, %(i)))
    case (%(i), x)              => Set((i, x))
    case (x, %(j))              => Set((j, x))
  }

  def replace(
      value: SimpleType,
      key: Int
  ): SimpleType => SimpleType = {
    case SimpleType(s, j) =>
      val s2 = s.map(replace(value, key))
      if (j == key) value.copy(sources = s2 ++ value.sources)
      else SimpleType(s2, j)
  }

  case class System(eqs: Set[(Int, SimpleType)]) {
    def equated: System =
      System((for {
        (i, u) <- eqs
        (j, v) <- eqs if j == i
        w <- analyze(u, v)
      } yield w) ++ eqs)

    def assess: Option[Set[Int]] = {
      def helper: List[(Int, SimpleType)] => Option[
        (Set[Int], Set[Int])
      ] = {
        case Nil => Some((Set.empty[Int], Set.empty[Int]))
        case (i, t) :: tail =>
          val ps = t.parameters
          if (ps.contains(i)) None
          else
            helper(tail).flatMap {
              case (l, r) => Some((l + i, r ++ ps))
            }
      }

      helper(eqs.toList) map { case (l, r) => l intersect r }
    }

    def eliminate(index: Int): System = {
      val (a, b) = eqs.partition { case (i, _) => i == index }
      val c = for {
        (_, u) <- a
        (j, v) <- b
      } yield (j, replace(u, index)(v))
      System(a ++ c).equated
    }

    def solve: Option[System] = assess match {
      case None => None
      case Some(indices) =>
        indices.headOption match {
          case None        => Some(this)
          case Some(index) => eliminate(index).solve
        }
    }
  }
}
