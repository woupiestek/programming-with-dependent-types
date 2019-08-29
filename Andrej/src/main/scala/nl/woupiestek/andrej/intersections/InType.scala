package nl.woupiestek.andrej.intersections

sealed trait InType

object InType {

  case class Parameter(index: Int) extends InType

  case class Arrow(source: Set[InType], target: InType)
      extends InType

  def replace(
      substitution: InType,
      index: Int
  ): InType => InType = {
    case Parameter(i) if i == index => substitution
    case Arrow(s, t) =>
      Arrow(
        s.map(replace(substitution, index)),
        replace(substitution, index)(t)
      )
    case other => other
  }

  def freeVars: InType => Set[Int] = {
    case Parameter(i) => Set(i)
    case Arrow(s, t)  => s.flatMap(freeVars) union freeVars(t)
  }

  def solve(
      equations: List[(InType, InType)],
      solution: Map[Int, InType]
  ): Option[Map[Int, InType]] = equations match {
    case Nil => Some(solution)
    case h :: t =>
      h match {
        case (Parameter(i), Parameter(j)) if i == j =>
          solve(t, solution)
        case (Parameter(i), a) if !freeVars(a)(i) =>
          solution.get(i) match {
            case Some(b) => solve((b, a) :: t, solution)
            case None =>
              val eqs = equations.map {
                case (b, c) =>
                  (replace(a, i)(b), replace(a, i)(c))
              }
              val sol = solution.map {
                case (j, b) => (j, replace(a, i)(b))
              } + (i -> a)
              solve(eqs, sol)
          }
        case (Parameter(i), a) if freeVars(a)(i) => None
        case (_, Parameter(_))                   => solve(h.swap :: t, solution)
        case (Arrow(a, b), Arrow(c, d)) => //take the excessive route again
          val e = for {
            f <- a
            g <- c
          } yield (f, g)
          solve((b, d) :: e.toList ++ t, solution)
      }
  }

}
