package nl.woupiestek.equalizer.simpler

import scala.language.higherKinds

object X {

  //todo: get rid of the strings here?
  case class Equations(
      position: Int,
      sameValue: Set[(Int, Int)] = Set.empty,
      sameType: Set[(Int, Int)] = Set.empty,
      freeVariables: Set[(Int, String)] = Set.empty,
      arrows: Set[(Int, Int, Int)] = Set.empty,
      abstractions: Set[(Int, String, Int)] = Set.empty,
      applications: Set[(Int, Int, Int)] = Set.empty,
      lets: Set[(Int, String, Int, Int)] = Set.empty,
      check: Set[(Int, Int, Int, Int)] = Set.empty
  )

  case class SimpleTypeEquations(
      equalities: Set[(Int, Int)] = Set.empty,
      arrows: Set[(Int, Int, Int)] = Set.empty
  )

  type T = (Int, Set[(String, Int)]) => Set[(String, Int)]

  //no increase in index in the output,
  //but something may be needed.
  def variable(name: String): T =
    (index, vars) => vars + ((name, index))

  /* issue: while reducing, variables and equations attached to them
   * must be taken seriously.
   */

  //use a state monad to track state through parsing

  object instance
      extends TermLike[String, Equations => Equations] {
    override def variable(
        id: String
    ): Equations => Equations =
      e =>
        e.copy(
          freeVariables = e.freeVariables + (
            (
              e.position,
              id
            )
          )
        )

    override def lambda(
        id: String,
        body: Equations => Equations
    ): Equations => Equations =
      e => {
        val f = body(e)
        val v = f.freeVariables.collect {
          case (x, y) if y == id => (x, f.position + 1)
        }
        f.copy(
          position = f.position + 2,
          freeVariables = f.freeVariables.filterNot(_._2 == id),
          sameValue = f.sameValue ++ v,
          sameType = f.sameType ++ v,
          arrows = f.arrows + (
            (
              f.position + 2,
              f.position + 1,
              e.position
            )
          ),
          abstractions = f.abstractions + (
            (
              f.position + 2,
              id,
              f.position
            )
          )
        )
      }

    override def operate(
        operator: Equations => Equations,
        operand: Equations => Equations
    ): Equations => Equations =
      e => {
        val f = operator(e)
        val g = operand(f)
        g.copy(
          position = g.position + 1,
          arrows = g.arrows + (
            (
              f.position,
              g.position,
              g.position + 1
            )
          ),
          applications = g.applications + (
            (
              g.position + 1,
              f.position,
              g.position
            )
          )
        )
      }

    override def let(
        id: String,
        value: Equations => Equations,
        context: Equations => Equations
    ): Equations => Equations =
      e => {
        val f = context(e)
        val g = value(f.copy(position = f.position + 1))
        val v = f.freeVariables.collect {
          case (x, y) if y == id => (x, f.position + 1)
        } + ((g.position, f.position + 1))
        g.copy(
          position = g.position + 1,
          freeVariables = g.freeVariables.filterNot(_._2 == id),
          sameValue = g.sameValue ++ v,
          sameType = g.sameType ++ v + (
            (
              g.position + 1,
              f.position
            )
          ),
          lets = g.lets + (
            (
              g.position + 1,
              id,
              g.position,
              f.position
            )
          )
        )
      }

    override def check(
        left: Equations => Equations,
        right: Equations => Equations,
        context: Equations => Equations
    ): Equations => Equations =
      e => {
        val f = context(e)
        val g = left(f)
        val h = right(g)
        h.copy(
          position = h.position + 1,
          sameValue = h.sameValue + (
            (
              f.position,
              g.position
            )
          ),
          sameType = h.sameType + ((f.position, g.position)) + (
            (
              h.position + 1,
              f.position
            )
          ),
          check = h.check + (
            (
              h.position + 1,
              f.position,
              g.position,
              h.position
            )
          )
        )
      }
  }

}

trait Builder[F[_], P] {
  def position: F[P]

  def unify(other: P): F[Unit]

  def arrow(dom: P, cod: P): F[Unit]

  def variable(name: String): F[Unit]

  def lambda(name: String, body: P): F[Unit]

  def application(operator: P, operand: P): F[Unit]

  def let(name: String, value: P, context: P): F[Unit]

  def check(left: P, right: P, context: P): F[Unit]
}

object Builder {

  sealed abstract class Term[P]

  case class Variable[P](position: P, name: String)

}
