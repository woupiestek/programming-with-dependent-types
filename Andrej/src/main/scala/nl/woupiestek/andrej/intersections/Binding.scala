package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType.{ ->:, Atomic }
import nl.woupiestek.andrej.intersections.Lattice._

object Binding {

  sealed trait B

  case class Upper(index: Int, y: RType) extends B

  case class Lower(y: RType, int: Int) extends B

  def leq(x: LType, y: LType): Set[Set[B]] = join(x.rTypes.map(xr => meet(y.rTypes.map(leq(xr, _)))))

  def leq(x: RType, y: RType): Set[Set[B]] = (x, y) match {
    case (a ->: b, c ->: d) => Lattice.meet(leq(c, a), leq(b, d))
    case (Atomic(a), Atomic(b)) if a == b => top
    case (Atomic(Parameter(i)), _) => point(Upper(i, y))
    case (_, Atomic(Parameter(i))) => point(Lower(y, i))
    case _ => bottom
  }

  def combine(x: LType, y: List[LType], p: Set[Set[B]]): LType = LType(x.rTypes.flatMap(combine(_, y, p)).headOption.toSet)

  def fetch(i: Int, p: Set[Set[B]]): (Option[RType], Set[Set[B]]) = {
    p.flatMap {
      q =>
        val ub = LType(q.collect { case Upper(j, y) if j == i => y })
        val (t, r) = q.collect { case Lower(y, j) if j == i => y }.foldLeft[(Option[RType], Set[Set[B]])]((Some(Atomic(Parameter(i))), top)) {
          case ((Some(a), b), c) => unify(a, c) match {
            case (d, e) => (d, meet(e, b))
          }
          case ((None, a), b) => (None, bottom)
        }
        t.map((_, leq(LType(t.toSet), ub)))
    }.headOption match {
      case Some((g, h)) => (Some(g), h)
      case None => (None, bottom)
    }
  }

  def unify(x: RType, y: RType): (Option[RType], Set[Set[B]]) = (x, y) match {
    case (a ->: b, c ->: d) => unify(b, d) match {
      case (rt, q) => (rt.map(t => ->:(LType(a.rTypes union c.rTypes), t)), q)
    }
    case (Atomic(a), Atomic(b)) if a == b => (Some(x), top)
    case (Atomic(Parameter(i)), _) => (Some(y), meet(point(Upper(i, y)), point(Lower(y, i))))
    case (_, Atomic(Parameter(i))) => (Some(x), meet(point(Upper(i, x)), point(Lower(x, i))))
    case _ => (None, bottom)
  }

  def combine(x: RType, y: List[LType], p: Set[Set[B]]): Option[RType] = (x, y) match {
    case (a, Nil) => Some(a)
    case (a ->: b, c :: d) => combine(b, d, meet(leq(c, a), p))
    case (Atomic(Parameter(i)), _) => fetch(i, p) match {
      case (Some(z), q) => combine(z, y, q)
      case (None, _) => None
    }
    case _ => None
  }

}

object Lattice {
  def point[P](p: P): Set[Set[P]] = Set(Set(p))

  def bottom[P]: Set[Set[P]] = Set.empty[Set[P]]

  def join[P](x: Set[Set[P]], y: Set[Set[P]]): Set[Set[P]] = x union y

  def join[P](x: Set[Set[Set[P]]]): Set[Set[P]] = x.foldLeft(bottom[P])(join[P])

  def top[P]: Set[Set[P]] = Set(Set.empty[P])

  def meet[P](x: Set[Set[P]], y: Set[Set[P]]): Set[Set[P]] = for {
    a <- x
    b <- y
  } yield a union b

  def meet[P](x: Set[Set[Set[P]]]): Set[Set[P]] = x.foldLeft(top[P])(meet[P])
}

