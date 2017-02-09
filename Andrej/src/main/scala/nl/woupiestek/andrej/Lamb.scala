package nl.woupiestek.andrej

import scala.annotation.tailrec

sealed trait Lamb

object Lamb {

  case class Vari(index: Int) extends Lamb

  case class Appl(operator: Lamb, operands: List[Lamb]) extends Lamb

  case class Subs(term: Lamb, context: List[Lamb]) extends Lamb

  case class Abst(dom: Lamb, body: Lamb) extends Lamb

  case class Prod(dom: Lamb, body: Lamb) extends Lamb

  case object Univ extends Lamb

  def fold[E](l: Lamb, e: DeBruijnExpr[E]): E = l match {
    case Vari(i) => e.get(i)
    case Appl(x, y) => y.foldLeft(fold(x, e)) { case (a, b) => e.application(a, fold(b, e)) }
    case Subs(x, y) => y.foldLeft(fold(x, e)) { case (a, b) => e.push(fold(b, e), a) }
    case Abst(x, y) => e.lambda(fold(x, e), fold(y, e))
    case Prod(x, y) => e.pi(fold(x, e), fold(y, e))
    case Univ => e.omega
  }

  def product(dom: Lamb, body: Lamb, context: List[Lamb]) = Some(Subs(Prod(dom, body), context))

  def tuple(index: Int, args: List[Lamb]) = Some(Appl(Vari(index), args))

  def closure(dom: Lamb, body: Lamb, context: List[Lamb]) = Some(Subs(Abst(dom, body), context))

  object Instance extends DeBruijnExpr[Lamb] {
    override def get(index: Int): Lamb = Vari(index)

    override def push(value: Lamb, context: Lamb): Lamb = context match {
      case Subs(x, y) => Subs(x, y ++ List(value))
      case _ => Subs(context, List(value))
    }

    override def application(operator: Lamb, operand: Lamb): Lamb = operator match {
      case Appl(x, y) => Appl(x, y ++ List(operand))
      case _ => Appl(operator, List(operand))
    }

    override def lambda(dom: Lamb, value: Lamb): Lamb = Abst(dom, value)

    override def omega: Lamb = Univ

    override def pi(dom: Lamb, fun: Lamb): Lamb = Prod(dom, fun)
  }

  def reduce(pivot: Lamb, context: List[Lamb], args: List[Lamb]): Option[Lamb] = pivot match {
    case Vari(i) if i < context.length => reduce(context(i), Nil, args)
    case Vari(i) if i >= context.length => tuple(i - context.length, args)
    case Appl(operator, operands) => reduce(operator, context, operands.map(Subs(_, context)) ++ args)
    case Subs(term, context2) => reduce(term, context2 ++ context, args)
    case Abst(dom, body) => args match {
      case head :: tail if typeCheck(dom, head) => reduce(body, head :: context, tail)
      case Nil => closure(dom, body, context)
    }
    case Prod(x, y) if args.isEmpty => product(x, y, context)
    case Univ if args.isEmpty => Some(Univ)
    case _ => None
  }

  def typeCheck(dom: Lamb, head: Lamb): Boolean = {
    @tailrec def typeCheck2(term: Lamb, cont: List[Lamb], typ: Lamb): Boolean = term match {
      case Vari(i) => cont lift i exists (unifiable(_, typ))
      case Abst(x, y) => typeCheck2(y, x :: cont, Prod(x, typ))
      case Appl(x, y) => traverse(y)(z => typeOf(z, cont)) match {
        case None => false
        case Some(a) => typeCheck2(x, cont, a.foldLeft(typ) { case (b, c) => Prod(b, c) })
      }
      case Subs(x, y) => traverse(y)(z => typeOf(z, cont)) match {
        case None => false
        case Some(a) => typeCheck2(x, a ++ cont, typ)
      }
      case Prod(x, y) if unifiable(typ, Univ) => typeCheck2(y, x :: cont, Univ)
      case _ => false
    }

    typeCheck2(head, Nil, dom)
  }

  def unifiable(x: Lamb, y: Lamb): Boolean = {
    @tailrec def g(z: List[(Lamb, Lamb)]): Boolean = {
      def f(x: Lamb, y: Lamb): Option[List[(Lamb, Lamb)]] = for {
        xx <- reduce(x, Nil, Nil)
        yy <- reduce(y, Nil, Nil)
        zz <- (xx, yy) match {
          case (Appl(a, b), Appl(c, d)) if a == c && b.length == d.length => Some(b.zip(d))
          case (Subs(Abst(a, b), c), Subs(Abst(d, e), f)) => Some((Subs(a, c), Subs(d, f)) :: (Subs(b, c), Subs(e, f)) :: Nil)
          case (Subs(Prod(a, b), c), Subs(Prod(d, e), f)) => Some((Subs(a, c), Subs(d, f)) :: (Subs(b, c), Subs(e, f)) :: Nil)
          case (Univ, Univ) => Some(Nil)
          case _ => None
        }
      } yield zz

      z match {
        case Nil => true
        case (a, b) :: c => f(a, b) match {
          case None => false
          case Some(d) => g(d ++ c)
        }
      }
    }

    g((x, y) :: Nil)
  }

  def typeOf(term: Lamb, cont: List[Lamb]): Option[Lamb] = {
    @tailrec def project(pivot: Lamb, args: List[Lamb]): Option[Lamb] = {
      val tp = reduce(pivot, Nil, Nil)
      (tp, args) match {
        case (Some(Subs(Prod(a, b), c)), head :: tail) if typeCheck(head, a) => project(Subs(b, c), tail)
        case (Some(Subs(Prod(_, _), _)), Nil) => tp
        case _ => None
      }
    }

    reduce(term, Nil, Nil) flatMap {
      case Appl(Vari(i), y) => for {
        c <- cont lift i
        p <- project(c, y)
      } yield p
      case Subs(Abst(a, b), c) => Some(Subs(Prod(a, b), c))
      case Subs(Prod(_, _), _) => Some(Univ)
      case _ => None
    }
  }

  private def traverse[X, Y](x: List[X])(f: X => Option[Y]): Option[List[Y]] = x match {
    case Nil => Some(Nil)
    case h :: t => for {
      h2 <- f(h)
      t2 <- traverse(t)(f)
    } yield h2 :: t2
  }

}