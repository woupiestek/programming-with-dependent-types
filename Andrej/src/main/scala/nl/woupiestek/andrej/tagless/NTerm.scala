package nl.woupiestek.andrej.tagless

sealed trait NTerm

object NTerm {

  case class Appl(index: Int, operands: List[NTerm]) extends NTerm

  sealed trait BindingKind

  case object Lambda extends BindingKind

  case object Pi extends BindingKind

  case class Bound(kind: BindingKind, dom: NTerm, fun: NTerm) extends NTerm

  case class Univ(index: Int) extends NTerm

  case object Fail extends NTerm

  case class Context(entries: List[NTerm]) {

    def reduce(kind: BindingKind, ti: NTerm, ops: List[NTerm]): NTerm = ops.foldLeft(ti) {
      case (head, Bound(k, d, f)) if k == kind && typeOf(head) == d => grab(head, 0)(f)
      case _ => Fail
    }

    def grab(head: NTerm, i: Int): NTerm => NTerm = {
      def g(i: Int): NTerm => NTerm = {
        case Appl(j, ops) =>
          val ops2 = ops.map(g(i))
          if (i == j) reduce(Lambda, head, ops2)
          else if (i < j) Appl(j - 1, ops2)
          else Appl(j, ops2)
        case Bound(k, d, f) => Bound(k, g(i)(d), g(i + 1)(f))
        case constant => constant
      }

      g(i)
    }

    def typeOf: NTerm => NTerm = {
      case Appl(i, os) => reduce(Pi, entries.lift(i).getOrElse(Fail), os)
      case Bound(k, a, b) =>
        val c = Context(a :: entries).typeOf(b)
        k match {
          case Lambda => Bound(Pi, a, c)
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