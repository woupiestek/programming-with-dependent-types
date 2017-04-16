package nl.woupiestek.andrej.intersections

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
    case Arrow(a, b) if subtypes(a, y, 0) => b
    case _ => top
  }

  def ponens(x: InType, ys: List[InType]): InType = ys.foldLeft(x)(ponens)
}
