package nl.woupiestek.andrej.intersections

case class LType(rTypes: Set[RType]) {
  def &(other: LType): LType = copy(rTypes = rTypes union other.rTypes)

  override def toString: String = if (rTypes.isEmpty) "T" else rTypes.mkString(" & ")
}

case class RType(sources: List[LType], target: AType) {

  def ->:(source: LType): RType = copy(sources = source :: sources)

  override def toString: String =
    if (sources.isEmpty) target.toString
    else (sources ++ List(target)).mkString("(", " -> ", ")")
}

sealed trait AType

case class Constant(name: String) extends AType {
  override def toString: String = name
}

case class Parameter(index: Int) extends AType {
  override def toString: String = index.toString
}

object LType {

  def parameter(index: Int): LType = LType(Set(Atomic(Parameter(index))))

  def constant(name: String): LType = LType(Set(Atomic(Constant(name))))

  def omega = LType(Set.empty)

  def intersection(x: LType, y: LType) = LType(x.rTypes union y.rTypes)

  def arrow(x: LType, y: LType) = LType(y.rTypes.map { case RType(sy, ty) => RType(x :: sy, ty) })

  def left(x: RType): LType = LType(Set(x))

  def replace(lType: LType, index: Int, sub: LType): LType = LType(lType.rTypes.flatMap(replace(_, index, sub).rTypes))

  def replace(rType: RType, index: Int, sub: LType): LType = rType match {
    case a ->: b => arrow(replace(a, index, sub), replace(b, index, sub))
    case Atomic(a) => replace(a, index, sub)
  }

  def replace(aType: AType, index: Int, sub: LType): LType = aType match {
    case Constant(name) => constant(name)
    case Parameter(j) if j < index => parameter(j)
    case Parameter(j) if j == index => sub
    case Parameter(j) if j > index => parameter(j - 1)
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

  //typing without elimination
  def leq(x: LType, t: AType): Boolean = x.rTypes.contains(Atomic(t))

  def leq(x: LType, y: RType): Boolean = y match {
    case a ->: b => leq(LType(x.rTypes.collect { case c ->: d if leq(a, b) => d }), b)
    case Atomic(_) => x.rTypes.contains(y)
  }

  def leq(x: LType, y: LType): Boolean = y.rTypes.forall(leq(x, _))

  def combine(x: LType, y: List[LType]): LType = LType(x.rTypes.flatMap(combine(_, y)).headOption.toSet)

  def combine(x: RType, y: List[LType]): Option[RType] = y.foldLeft[Option[RType]](Some(x)) {
    case (Some(a ->: b), c) if leq(c, a) => Some(b)
    case _ => None
  }
}

