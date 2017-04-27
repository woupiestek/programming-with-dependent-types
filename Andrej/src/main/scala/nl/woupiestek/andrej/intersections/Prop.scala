package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType._

sealed trait Prop

object Prop {

  case class UB(index: Int, inType: InType) extends Prop

  case class LB(inType: InType, index: Int) extends Prop

  object False extends Prop

  def overBind(index: Int, inType: InType): Set[Prop] = Set {
    if (freeVars(inType).contains(index)) False else UB(index, inType)
  }

  def underBind(inType: InType, index: Int): Set[Prop] = Set {
    if (freeVars(inType).contains(index)) False else LB(inType, index)
  }

  val fail: Set[Prop] = Set(False)

  def exists(props: Set[Prop]): Set[Prop] = {
    val ub = intersection(props.collect { case UB(0, t) => t })
    props.flatMap {
      case LB(t, i) if i > 0 => underBind(replace(t, ub), i - 1)
      case UB(i, t) if i > 0 => overBind(i - 1, replace(t, ub))
      case LB(t, 0) => leq(t, ub)
      case _ => Set.empty[Prop]
    }
  }

  def leq(x: InType, y: InType): Set[Prop] = x match {
    case Var(i) if y == Var(i) => Set.empty
    case Var(i) if y != Var(i) => overBind(i, y)
    case Intersection(ts) => ts.map(leq(_, y)).find(s => !s.contains(False)).getOrElse(Set(False))
    case Arrow(a, b) => y match {
      case Var(i) if !freeVars(x).contains(i) => underBind(x, i)
      case Intersection(ts) => ts.flatMap(leq(x, _))
      case Arrow(c, d) => leq(c, a) union leq(b, d)
      case _ => fail
    }
    case _ => fail
  }

  private def deduce(props: Set[Prop], context: InType): InType = {
    def deduceLeft(context2: InType): Set[InType] = context2 match {
      case Var(i) => props.collect { case LB(t, j) if i == j => t }
      case Intersection(ts) => ts.foldLeft(Set.empty[Set[InType]]) {
        case (x, y) => for {
          a <- x
          b <- deduceLeft(y)
        } yield a + b
      }.map(intersection)
      case Arrow(a, b) => deduceLeft(b).map(arrow(deduce(props, a), _))
    }

    if (props.contains(False)) top else context match {
      case Var(i) => intersection(props.collect { case UB(j, t) if i == j => t })
      case Intersection(ts) => intersection(ts.map(deduce(props, _)))
      case Arrow(a, b) => intersection(deduceLeft(a).map(arrow(_, deduce(props, b))))
    }
  }

  def combine(x: InType, y: InType): InType = x match {
    case Intersection(xs) => intersection(xs.map(combine(_, y)))
    case Arrow(a, b) => deduce(leq(y, a), b)
    case _ => top
  }

  def combine(x: InType, y: List[InType]): InType = y.foldLeft(x) { case (a, c) => combine(a, c) }

}