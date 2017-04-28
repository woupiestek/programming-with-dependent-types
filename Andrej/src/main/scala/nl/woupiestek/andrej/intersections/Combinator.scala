package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._

// wat er
object Combinator {

  def meet(x: Option[Map[Int, LType]], y: Option[Map[Int, LType]]): Option[Map[Int, LType]] = for (a <- x; b <- y) yield a ++ b //not there yet

  def leq(x: LType, y: AType): Option[Map[Int, LType]] = y match {
    case _ if x.rTypes.contains(Atomic(y)) => Some(Map.empty)
    case Parameter(i) => Some(Map(i -> x))
    case _ => None
  }

  def leq(x: LType, y: RType): Option[Map[Int, LType]] = leq(combine(x, y.sources), y.target)

  def leq(x: LType, y: LType): Option[Map[Int, LType]] = y.rTypes.map(leq(x, _)).foldLeft[Option[Map[Int, LType]]](Some(Map.empty))(meet)

  def replace(x: RType, bounds: Map[Int, LType]): LType = x match {
    case a ->: b => arrow(replace(a, bounds), replace(b, bounds))
    case Atomic(Constant(_)) => left(x)
    case Atomic(Parameter(i)) => bounds.getOrElse(i, left(x))
  }

  def replace(x: LType, bounds: Map[Int, LType]): LType = x.rTypes.map(replace(_, bounds)).foldLeft(omega)(intersection)

  def combine(x: LType, y: List[LType]): LType = {
    def helper(x: RType, y: List[LType]): LType = y match {
      case Nil => left(x)
      case c :: d => x match {
        case a ->: b => leq(c, a) match {
          case Some(e) => combine(replace(b, e), d.map(replace(_, e)))
          case None => omega
        }
        case _ => omega
      }
    }

    x.rTypes.map(helper(_, y)).foldLeft(omega)(intersection)
  }

}
