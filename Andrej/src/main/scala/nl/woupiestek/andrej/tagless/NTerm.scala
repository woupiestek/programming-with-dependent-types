package nl.woupiestek.andrej.tagless

sealed trait NTerm[+T]

object NTerm {

  case class Appl[T](index: Int, operands: List[T]) extends NTerm[T]

  sealed trait BindingKind

  case object Lambda extends BindingKind

  case object Pi extends BindingKind

  case class Bound[T](kind: BindingKind, dom: T, fun: T) extends NTerm[T]

  case class Univ(index: Int) extends NTerm[Nothing]

  case object Fail extends NTerm[Nothing]

  trait Term[T] {
    def fold(nt: => NTerm[T]): T

    def unfold(t: T): NTerm[T]
  }

  case class Context[T](entries: List[T])(implicit T: Term[T]) {

    def same(term: NTerm[T], term2: T): Boolean = (term, T.unfold(term2)) match {
      case (Appl(i, as), Appl(j, bs)) if i == j && as.length == bs.length =>
        as.map(T.unfold).zip(bs).forall { case (a, b) => same(a, b) }
      case (Bound(a, b, c), Bound(d, e, f)) if a == d =>
        same(T.unfold(b), e) && same(T.unfold(c), f)
      case (Univ(i), Univ(j)) => i == j
      case _ => false
    }

    def reduce(kind: BindingKind, ti: T, ops: List[T]): NTerm[T] = ops.foldLeft(T.unfold(ti)) {
      case (operator, operand) => operator match {
        case Bound(k, d, f) if k == kind && same(typeOf(operand), d) => grab(operand, 0)(f)
        case _ => Fail
      }
    }

    def grab(head: T, i: Int): T => NTerm[T] = {
      def g(i: Int)(t: T): NTerm[T] = T.unfold(t) match {
        case Appl(j, ops) =>
          val ops2 = ops.map(op => T.fold(g(i)(op)))
          if (i == j) reduce(Lambda, head, ops2)
          else if (i < j) Appl(j - 1, ops2)
          else Appl(j, ops2)
        case Bound(k, d, f) => Bound(k, T.fold(g(i)(d)), T.fold(g(i + 1)(f)))
        case constant => constant
      }

      g(i)
    }

    def typeOf(t: T): NTerm[T] = T.unfold(t: T) match {
      case Appl(i, os) => reduce(Pi, entries.lift(i).getOrElse(T.fold(Fail)), os)
      case Bound(k, a, b) =>
        val c = Context(a :: entries).typeOf(b)
        k match {
          case Lambda => Bound(Pi, a, T.fold(c))
          case Pi => (typeOf(a), c) match {
            case (Univ(i), Univ(j)) => Univ(math.max(i, j))
            case _ => Fail
          }
        }
      case Univ(i) => Univ(i + 1)
      case Fail => Fail
    }
  }

}