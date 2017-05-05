package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._

object Combinator {

  def leq(x: LType, y: AType): RType => LType = z => y match {
    case _ if x.rTypes.contains(Atomic(y)) => left(z)
    case Parameter(i) => LType.replace(z, i, x)
    case _ => omega
  }

  def leq(x: LType, y: RType): RType => LType =
    leq(combine(x, y.sources), y.target)

  def leq(x: LType, y: LType): RType => LType =
    z => y.rTypes.map(leq(x, _)(z)).foldLeft[LType](omega)(intersection)

  def combine(x: LType, y: List[LType]): LType = {
    def helper(x: RType, y: List[LType]): LType = y match {
      case Nil => left(x)
      case c :: d => x match {
        case a ->: b => leq(c, a)(b)
        case _ => omega
      }
    }

    x.rTypes.map(helper(_, y)).foldLeft(omega)(intersection)
  }

}

object Combinator2 {

  def leq(x: LType, y: LType)(z: RType): Set[RType] = y.rTypes.flatMap {
    case a ->: b => x.rTypes.flatMap {
      case c ->: d => leq(LType(leq(c, a)(d)), left(b))(z)
      case Atomic(Parameter(i)) => replace(i, y)(z)
      case Atomic(Constant(_)) => Set.empty[RType]
    }
    case Atomic(Parameter(i)) => replace(i, x)(z)
    case Atomic(Constant(_)) => Set.empty[RType]
  }

  def replace(key: Int, value: LType): RType => Set[RType] = {
    case a ->: b => replace(key, value)(b).map(LType(a.rTypes.flatMap(replace(key, value))) ->: _)
    case Atomic(Parameter(i)) if i == key => value.rTypes
    case other => Set(other)
  }

}

