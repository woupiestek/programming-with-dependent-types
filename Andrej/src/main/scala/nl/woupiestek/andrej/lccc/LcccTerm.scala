package nl.woupiestek.andrej.lccc

sealed trait LcccTerm

object LcccTerm {

  case class Com(path: List[LcccTerm]) extends LcccTerm

  case class App(operator: LcccTerm, operand: List[LcccTerm]) extends LcccTerm

  case class Lam(dom: LcccTerm, body: LcccTerm) extends LcccTerm

  case class Arr(ante: LcccTerm, cons: LcccTerm) extends LcccTerm

  def same(x: LcccTerm, y: LcccTerm): Boolean = {
    (reduce(x, Nil, Nil), reduce(y, Nil, Nil)) match {
      case (Some(App(Com(Nil), b)), Some(App(Com(Nil), d))) if b.length == d.length =>
        b.zip(d).forall { case (e, f) => same(e, f) }
      case (Some(App(Arr(a, b), c)), Some(App(Arr(d, e), f))) if c.length == f.length =>
        same(a, d) && same(b, e) && c.zip(f).forall { case (g, h) => same(g, h) }
      case (Some(Com(Lam(a, b) :: c)), Some(Com(Lam(d, e) :: f))) =>
        same(Com(a :: c), Com(d :: f)) && same(Com(b :: c), Com(e :: f))
      case _ => false
    }
  }

  def reduce(pivot: LcccTerm, context: List[LcccTerm], args: List[LcccTerm]): Option[LcccTerm] = pivot match {
    case Com(p) => p ++ context match {
      case Nil => Some(App(Com(Nil), args))
      case h :: t => reduce(h, t, args)
    }
    case App(x, y) => reduce(x, context, y.map(z => Com(z :: context)) ++ args)
    case Lam(x, y) => args match {
      case Nil => Some(Com(pivot :: context))
      case h :: t if same(Com(x :: h :: Nil), Com(Nil)) => reduce(y, h :: context, t)
    }
    case Arr(x, y) => context match {
      case Nil => Some(App(pivot, args))
      case h :: t => for {
        Com(Lam(a, b) :: c) <- reduce(h, t, Nil) if same(a, x)
        z <- reduce(y, b :: c, args)
      } yield z
    }
  }

}