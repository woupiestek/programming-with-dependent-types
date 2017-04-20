package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType._

sealed trait Prop

object Prop {

  case class UB(index: Int, inType: InType) extends Prop

  case class LB(inType: InType, index: Int) extends Prop

  object False extends Prop

  def overBind(index: Int, inType: InType): Prop = {
    if (freeVars(inType).contains(index)) False else UB(index, inType)
  }

  def underBind(inType: InType, index: Int): Prop = {
    if (freeVars(inType).contains(index)) False else LB(inType, index)
  }

  def exists(props: Set[Prop]): Set[Prop] = {
    val ub = intersection(props.collect { case UB(0, t) => t })
    val r: Set[Prop] = props.collect {
      case LB(t, i) if i > 0 => underBind(replace(t, ub), i - 1)
      case UB(i, t) if i > 0 => overBind(i - 1, replace(t, ub))
    }
    val p = props.flatMap {
      case LB(t, 0) => leq(t, ub)
      case _ => Set.empty[Prop]
    }
    p union r
  }

  def leq(x: InType, y: InType): Set[Prop] = {
    x match {
      case Var(i) if y == Var(i) => Set.empty
      case Var(i) if y != Var(i) => Set(overBind(i, y))
      case Intersection(ts) => ts.map(leq(_, y)).find(s => !s.contains(False)).getOrElse(Set(False))
      case Forall(t) => exists(leq(t, insert(y)))
      case Arrow(a, b) => y match {
        case Var(i) if !freeVars(x).contains(i) => Set(underBind(x, i))
        case Intersection(ts) => ts.flatMap(leq(x, _))
        case Forall(t) => leq(insert(x), t)
        case Arrow(c, d) => leq(c, a) union leq(b, d)
        case _ => Set.empty[Prop]
      }
    }
  }

  def combine(x: InType, y: InType): InType = x match {
    case Intersection(xs) => intersection(xs.map(combine(_, y)))
    case Forall(t) => Forall(combine(t, insert(y)))
    case Arrow(a, b) if !leq(y, a).contains(False) => b
    case _ => top
  }

  def combine(x: InType, y: List[InType]): InType = y.foldLeft(x) { case (a, c) => combine(a, c) }

}