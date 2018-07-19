package nl.woupiestek.andrej.equalambda

import nl.woupiestek.andrej.equalambda.Term._

import scala.annotation.tailrec

sealed trait Term

object Term {
  type Variable[Te] = String => Te
  type Let[Te] = (String, Te, Te) => Te
  type Application[Te] = (Te, Te) => Te
  type Abstraction[Te] = (String, Te) => Te
  type Check[Te] = (Te, Te, Te) => Te

  case class VarT(name: String) extends Term

  def variable: Variable[Term] = VarT

  case class Where(u: Term, t: Map[String, Term]) extends Term

  def let: Let[Term] = (x, y, z) => Where(z, Map(x -> y))

  case class AppT(t: Term, u: List[Term]) extends Term

  def application: Application[Term] = (t, u) => AppT(t, List(u))

  case class AbsT(key: String, t: Term) extends Term

  def abstraction: Abstraction[Term] = AbsT

  case class CheT(t: Term, u: Term, v: Term) extends Term

  def check: Check[Term] = CheT

  sealed trait WHNF

  case class AppW(name: String, args: List[Term]) extends WHNF

  case class AbsW(name: String, body: Term, context: Map[String, Term]) extends WHNF

  @tailrec def eval(pivot: Term, context: Map[String, Term], stack: List[Term]): WHNF = pivot match {
    case VarT(name) => context.get(name) match {
      case None => AppW(name, stack)
      case Some(z) => eval(z, Map.empty, stack)
    }
    case Where(x, y) => eval(x, context ++ y.mapValues(Where(_, context)), stack)
    case AppT(x, y) => eval(x, context, y.map(Where(_, context)) ++ stack)
    case AbsT(x, y) => stack match {
      case Nil => AbsW(x, y, context)
      case h :: t => eval(y, context + (x -> h), t)
    }
    case CheT(x, y, z) => ??? //feels like it should try to verify that x = y
    //it should be a form of smart counter example search
    //Huet's higher unification?
  }

  def are(x: List[(WHNF, WHNF)]): Boolean = {
    x match {
      case Nil => true
      case (a, b) :: c if a == b => are(c)
      case (AppW(a, b), AppW(c, d)) :: e => a == c && b.length == d.length && {
        val b2 = b.map(eval(_, Map.empty, Nil))
        val d2 = d.map(eval(_, Map.empty, Nil))
        are((b2 zip d2) ++ e)
      }
      case (AbsW(a, b, c), AbsW(d, e, f)) :: g =>
        val b2 = eval(b, c - a, Nil)
        val c2 = eval(e, f + (d -> VarT(a)), Nil)
        are((b2, c2) :: g)
        //CheT cases?
      case _ => false
    }
  }

}

sealed trait Type

object Type {

  case class Parameter(index: Int, context: Map[String, Int]) extends Type

  case class Product(name: String, operator: Type, operand: Type) extends Type

  case class Arrow(operator: Type, operand: Type) extends Type

  case class Equals[Te](t: Te, u: Te) extends Type

  type T = M[Term]

  case class M[X](run: Context => (Context, X)) {
    def flatMap[Y](f: X => M[Y]): M[Y] = M { context =>
      val (cr, tc) = run(context)
      f(tc).run(cr)
    }

    def map[Y](f: X => Y): M[Y] = M { context =>
      val (cr, tc) = run(context)
      (cr, f(tc))
    }
  }

  def parameter = M(c => (c.copy(index = c.index + 1), Parameter(c.index, c.indices)))

  def equate(t: Type, u: Type) = M(c => (c.copy(equations = (t, u) :: c.equations), ()))

  def variable: Variable[T] = (name: String) => M { (context: Context) =>
    val c = context.indices.get(name).map(j =>
      context.copy(equations = (Parameter(context.index, context.indices), Parameter(j, context.indices)) :: context.equations)
    ).getOrElse(context.copy(indices = context.indices + (name -> context.index)))
    (c, Term.variable(name))
  }

  def let: Let[T] = (key: String, t: T, u: T) => for {
    i1 <- parameter
    _ <- variable(key) //wrong!
    i2 <- parameter
    _ <- equate(i1, i2)
    t0 <- t
    i3 <- parameter
    u0 <- u
    i0 <- parameter
    _ <- equate(i0, i3)
  } yield Term.let(key, t0, u0)

  def application: Application[T] = (t: T, u: T) => for {
    i1 <- parameter
    t0 <- t
    i2 <- parameter
    u0 <- u
    i0 <- parameter
    _ <- equate(i1, Arrow(i2, i0))
  } yield Term.application(t0, u0)

  def abstraction: Abstraction[T] = (key: String, t: T) => for {
    i1 <- parameter
    _ <- variable(key) //wrong!
    i2 <- parameter
    t0 <- t
    i0 <- parameter
    _ <- equate(i0, Product(key, i1, i2))
  } yield Term.abstraction(key, t0)

  def check: Check[T] = (t: T, u: T, v: T) => for {
    i1 <- parameter
    tt <- t
    i2 <- parameter
    _ <- equate(i1, i2)
    tu <- u
    i3 <- parameter
    _ <- equate(i3, Equals(tt, tu))
    i4 <- parameter
    tv <- v
    i0 <- parameter
    _ <- equate(i0, Arrow(i3, i4))
  } yield Term.check(tt, tu, tv)

  //Indices are needed because types are context dependent
  //We cannot assign a unique type to a term if it occurs more than once
  case class Context(
    equations: List[(Type, Type)],
    indices: Map[String, Int],
    index: Int)

}