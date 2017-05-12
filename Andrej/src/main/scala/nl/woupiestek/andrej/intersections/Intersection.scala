package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.AType.{ Constant, Parameter }

case class LType(rTypes: Set[RType]) {
  def &(other: LType): LType = copy(rTypes = rTypes union other.rTypes)

  override def toString: String = if (rTypes.isEmpty) "T" else rTypes.mkString(" & ")
}

case class RType(sources: List[LType], target: AType) {
  def replace(i: Int, x: RType): RType = {
    val s2 = sources.map(s => LType(s.rTypes.map(_.replace(i, x))))
    if (target == Parameter(i)) x.copy(sources = s2 ++ x.sources) else copy(sources = s2)
  }

  def ->:(source: LType): RType = copy(sources = source :: sources)

  override def toString: String =
    if (sources.isEmpty) target.toString
    else (sources ++ List(target)).mkString("(", " -> ", ")")
}

sealed trait AType

object AType {

  case class Constant(name: String) extends AType {
    override def toString: String = name
  }

  case class Parameter(index: Int) extends AType {
    override def toString: String = index.toString
  }

}

object LType {

  def parameter(index: Int): LType = left(Atomic(Parameter(index)))

  def constant(name: String): LType = left(Atomic(Constant(name)))

  def omega = LType(Set.empty)

  def intersection(x: LType, y: LType) = LType(x.rTypes union y.rTypes)

  def arrow(x: LType, y: LType) = LType(y.rTypes.map { case RType(sy, ty) => RType(x :: sy, ty) })

  def left(x: RType): LType = LType(Set(x))
  def left(x: Option[RType]): LType = LType(x.toSet)

  def replace(lType: LType, index: Int, sub: LType): LType = LType(lType.rTypes.flatMap(replace(_, index, sub).rTypes))

  def replace(rType: RType, index: Int, sub: LType): LType = rType match {
    case a ->: b => arrow(replace(a, index, sub), replace(b, index, sub))
    case Atomic(a) => replace(a, index, sub)
  }

  def replace(aType: AType, index: Int, sub: LType): LType = aType match {
    case Parameter(j) if j == index => sub
    case _ => left(Atomic(aType))
  }

  object Atomic {
    def apply(aType: AType): RType = RType(Nil, aType)

    def unapply(arg: RType): Option[AType] = if (arg.sources.isEmpty) Some(arg.target) else None
  }

  object ->: {
    def unapply(arrow: RType): Option[(LType, RType)] = arrow.sources match {
      case Nil => None
      case h :: t => Some((h, arrow.copy(sources = t)))
    }
  }

  def parameters(rType: RType): Set[Int] = rType.sources.toSet.flatMap((lt: LType) => lt.rTypes.flatMap(parameters)) ++
    (rType.target match {
      case Parameter(i) => Set(i)
      case _ => Set.empty
    })

  def solve(equations: List[(RType, RType)], types: Map[Int, RType]): Option[Map[Int, RType]] = equations match {
    case Nil => Some(types)
    case h :: t => h match {
      case (Atomic(Parameter(i)), Atomic(Parameter(j))) if i == j => solve(t, types)
      case (Atomic(Parameter(i)), x) if !parameters(x)(i) => types get i match {
        case Some(y) => solve((y, x) :: t, types)
        case None =>
          val equations2 = equations.map { case (a, b) => (a.replace(i, x), b.replace(i, x)) }
          val types2 = types.map { case (j, a) => (j, a.replace(i, x)) } + (i -> x)
          solve(equations2, types2)
      }
      case (_, Atomic(Parameter(_))) => solve(h.swap :: t, types)
      case (a ->: b, c ->: d) =>
        val z = for { //seems excessive
          e <- a.rTypes.toList
          f <- c.rTypes.toList
        } yield (f, e)
        solve((b, d) :: z ++ t, types)
      case _ => None
    }
  }

}

