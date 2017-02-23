package nl.woupiestek.andrej.lccc

import scala.collection.immutable.Nil

sealed trait LcccTerm

object LcccTerm {

  case class Get(index: Int) extends LcccTerm

  case class App(operator: LcccTerm, operand: LcccTerm) extends LcccTerm

  case class Lam(dom: LcccTerm, body: LcccTerm) extends LcccTerm

  case class Sub(body: LcccTerm, context: List[LcccTerm]) extends LcccTerm

  case class Pair(left: LcccTerm, right: LcccTerm) extends LcccTerm

  case class Pull(operator: LcccTerm, operand: LcccTerm) extends LcccTerm

  case class Arrow(ante: LcccTerm, cons: LcccTerm) extends LcccTerm

  def unify(y2: LcccTerm, x2: LcccTerm): Option[LcccTerm] = {

    def uSub(a: LcccTerm, b: List[LcccTerm], c: LcccTerm, d: List[LcccTerm]): Option[LcccTerm] = (a, c) match {
      case (Arrow(e, f), Arrow(g, h)) => for {
        i <- unify(Sub(e, b), Sub(g, b))
        j <- unify(Sub(f, d), Sub(h, d))
      } yield Arrow(i, j)
      case (Lam(e, f), Lam(g, h)) => for {
        i <- unify(Sub(e, b), Sub(g, b))
        j <- unify(Sub(f, d), Sub(h, d))
      } yield Lam(i, j)
      case (Pair(e, f), Pair(g, h)) => for {
        i <- unify(Sub(e, b), Sub(g, b))
        j <- unify(Sub(f, d), Sub(h, d))
      } yield Pair(i, j)
      case (Pull(e, f), Pull(g, h)) => for {
        i <- unify(Sub(e, b), Sub(g, b))
        j <- unify(Sub(f, d), Sub(h, d))
      } yield Pull(i, j)
      case _ => None
    }

    def uReduced(y2: LcccTerm, x2: LcccTerm): Option[LcccTerm] = (x2, y2) match {
      case (Get(i), Get(j)) if i == j => Some(Get(i))
      case (App(a, b), App(a2, b2)) => for {
        a3 <- uReduced(a, a2)
        b3 <- unify(b, b2)
      } yield App(a, b)
      case (Sub(a, b), Sub(c, d)) => uSub(a, b, c, d)
      case _ => None
    }

    (reduce(y2, Nil, Nil), reduce(x2, Nil, Nil)) match {
      case (Some(x), Some(y)) => uReduced(x, y)
      case _ => None
    }

  }

  def reduce(term: LcccTerm, context: List[LcccTerm], args: List[LcccTerm]): Option[LcccTerm] = {
    def halt = Some(Sub(term, context))

    term match {
      case Get(index) => Some(context lift index getOrElse args.foldLeft[LcccTerm](Get(index - context.length)) { case (x, y) => App(x, y) })
      case App(x, y) => reduce(x, context, Sub(y, context) :: args)
      case Lam(x, y) => args match {
        case Nil => halt
        case h :: t => for {
          _ <- reduce(x, context, h :: Nil) //no equation?
          z <- reduce(y, h :: context, t)
        } yield z
      }
      case Sub(x, y) => reduce(x, y ++ context, args)
      case Pair(x, y) => halt
      case Pull(a, b) => args match {
        case Nil => halt
        case h :: t => for {
          Pair(x, y) <- reduce(h, Nil, Nil)
          _ <- unify(App(Sub(a, context), x), App(Sub(b, context), y))
          z <- reduce(x, context, args)
        } yield z
      }
      case Arrow(x, y) => args match {
        case Nil => halt
        case h :: t => for {
          x2 <- reduce(x, context, Get(0) :: Nil)
          y2 <- reduce(y, context, App(h, Get(0)) :: Nil)
          _ <- unify(x2, y2)
          k <- reduce(h, context, t)
        } yield k
      }
      case _ => None
    }
  }
}