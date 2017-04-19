package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.Utils

sealed trait InType

object InType {

  case class Var(index: Int) extends InType

  case class Arrow private (source: InType, target: InType) extends InType

  case class Forall(inType: InType) extends InType

  case class Intersection private (types: Set[InType]) extends InType

  val top: InType = Intersection(Set.empty)

  def arrow(source: InType, target: InType): InType = target match {
    case Intersection(ts) => intersection(ts.map(arrow(source, _)))
    case _ => Arrow(source, target)
  }

  def arrow(sources: Traversable[InType], target: InType): InType = sources.foldRight(target)(arrow)

  def intersection(types: Set[InType]): InType = types.size match {
    case 0 => top
    case 1 => types.head
    case i if i > 1 => Intersection(types.flatMap {
      case Intersection(ts) => ts
      case t => Set(t)
    })
  }

  def forall(arity: Int, inType: InType): InType = (0 until arity).foldLeft(inType) { case (x, _) => Forall(x) }

  def insert(inType: InType, index: Int = 0): InType = inType match {
    case Var(i) if i < index => Var(i)
    case Var(i) if i >= index => Var(i + 1)
    case Arrow(s, t) => Arrow(insert(s, index), insert(t, index))
    case Forall(i) => Forall(insert(i, index + 1))
    case Intersection(ts) => intersection(ts.map(insert(_, index)))
  }

  def replace(inType: InType, substitution: InType, index: Int = 0): InType = inType match {
    case Var(i) if i < index => Var(i)
    case Var(i) if i == index => substitution
    case Var(i) if i > index => Var(i - 1)
    case Arrow(s, t) => Arrow(replace(s, substitution, index), replace(t, substitution, index))
    case Forall(i) => Forall(replace(i, substitution, index + 1))
    case Intersection(ts) => intersection(ts.map(replace(_, substitution, index)))
  }

  def subtypes(x: InType, y: InType, m: Int): Boolean = {
    def subArrow(z: InType, a: InType, b: InType, n: Int): Boolean = z match {
      case Intersection(cs) => cs.exists(subArrow(_, a, b, n))
      case Forall(c) => subArrow(c, a, b, n + 1)
      case Var(i) => i < n
      case Arrow(c, d) => subtypes(c, a, -n) && subtypes(b, d, n)
    }

    y match {
      case Intersection(bs) => bs.forall(subtypes(x, _, m))
      case Forall(a) => subtypes(insert(x), a, m)
      case Var(i) => (i + m < 0) || x == Var(i + m)
      case Arrow(a, b) => subArrow(x, a, b, m)
    }
  }

  def ponens(x: InType, y: InType): InType = x match {
    case Intersection(xs) => intersection(xs.map(ponens(_, y)))
    case Forall(t) => Forall(ponens(t, insert(y)))
    case Arrow(a, b) if subtypes(y, a, 0) => b
    case _ => top
  }

  def ponens(x: InType, ys: List[InType]): InType = ys.foldLeft(x)(ponens)

  sealed trait Prop

  case class UB(index: Int, inType: InType) extends Prop

  case class LB(inType: InType, index: Int) extends Prop

  def exists(props: Set[Prop]): Option[Set[Prop]] = {
    val lbs = props.collect { case LB(t, 0) => t }
    val ub = intersection(props.collect { case UB(0, t) => t })
    Utils.traverse(lbs.toList)(leq(_, ub)).map(_.flatten.toSet union props.collect {
      case LB(t, i) if i > 0 => LB(replace(t, ub), i - 1)
      case UB(i, t) if i > 0 => UB(i - 1, replace(t, ub))
    })
  }

  def leq(x: InType, y: InType): Option[Set[Prop]] = x match {
    case Var(i) if y == Var(i) => Some(Set.empty)
    case Var(i) if y != Var(i) => Some(Set(UB(i, y)))
    case Intersection(ts) => ts.foldLeft[Option[Set[Prop]]](None) { case (a, b) => a orElse leq(b, y) }
    case Forall(t) => for {
      p <- leq(t, insert(y))
      q <- exists(p)
    } yield q
    case Arrow(a, b) => y match {
      case Var(i) => Some(Set(LB(x, i)))
      case Intersection(ts) => Utils.traverse(ts.toList)(leq(x, _)).map(_.flatten.toSet)
      case Forall(t) => leq(insert(x), t)
      case Arrow(c, d) => for {
        e <- leq(c, a)
        f <- leq(b, d)
      } yield e union f
    }
  }

}
