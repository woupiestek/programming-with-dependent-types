package nl.woupiestek.andrej.masked

import scala.annotation.tailrec

sealed trait Rule[+X] {
  def map[Y](f: X => Y): Rule[Y]
}

case class Appl[+X](operator: X, operands: List[X]) extends Rule[X] {
  override def map[Y](f: (X) => Y): Rule[Y] = Appl(f(operator), operands map f)
}

case class Abst[+X](types: List[X], body: X) extends Rule[X] {
  override def map[Y](f: (X) => Y): Rule[Y] = Abst(types map f, f(body))
}

case class Bound(index: Int) extends Rule[Nothing] {
  override def map[Y](f: (Nothing) => Y): Rule[Y] = this
}

//Type level constructs
case class Type(level: Int) extends Rule[Nothing] {
  override def map[Y](f: (Nothing) => Y): Rule[Y] = this
}

case class Prod[+X](types: List[X], body: X) extends Rule[X] {
  override def map[Y](f: (X) => Y): Rule[Y] = Abst(types map f, f(body))
}

object Normalizer {
  sealed trait Expr[+V]
  case class Return[V](v: V) extends Expr[V]
  case class Suspend[+V](e: Rule[Expr[V]]) extends Expr[V]

  def redex[X](a: List[Expr[X]], b: Expr[X], c: List[Expr[X]]): Expr[X] = appl(abst(a, b), c)

  def appl[X](d: Expr[X], c: List[Expr[X]]): Expr[X] = if (Nil == c) d else Suspend(Appl(d, c))

  def abst[X](a: List[Expr[X]], b: Expr[X]): Expr[X] = if (Nil == a) b else Suspend(Abst(a, b))

  def bound[X](n: Int): Expr[X] = Suspend(Bound(n))

  def normalize[X](a: Expr[X]) = reduce(Nil, a, Nil, Nil)

  @tailrec def reduce[X](a: List[Expr[X]], b: Expr[X], c: List[Expr[X]], d: List[Expr[X]]): Expr[X] = b match {
    case Suspend(Bound(n)) if n < c.length => reduce(Nil, c(c.length - n - 1), Nil, d)
    case Suspend(Bound(n)) if n >= c.length =>
      val e: Expr[X] = bound(n - c.length)
      if (Nil == d) e else appl(e, d)
    case Suspend(Abst(x, y)) if d.length >= x.length =>
      val (head, tail) = d splitAt x.length
      reduce(a ++ x, y, c ++ head, tail)
    case Suspend(Appl(x, y)) =>
      val z = y map (redex(a, _, c))
      reduce(a, x, c, z ++ d)
    case _ => redex(a, b, c ++ d)
  }

  @tailrec def unifiable[X](x: List[Expr[X]], y: List[Expr[X]]): Boolean = (x, y) match {
    case (Nil, Nil) => true
    case (k :: l, m :: n) =>
      val u = normalize(k)
      val v = normalize(m)
      (u, v) match {
        case (Suspend(Appl(a, b)), Suspend(Appl(c, d))) => unifiable(a :: b ++ l, c :: d ++ n)
        case (Suspend(Abst(a, b)), Suspend(Abst(c, d))) => unifiable(a ++ (b :: l), c ++ (d :: n))
        case (Suspend(Prod(a, b)), Suspend(Prod(c, d))) => unifiable(a ++ (b :: l), c ++ (d :: n))
        case _ => u == v
      }
    case _ => false
  }

  def typ[X](n: Int) = Suspend(Type(n + 1))

  sealed trait InferenceResult[+E] {
    def flatMap[F](f: E => InferenceResult[F]): InferenceResult[F]
  }

  case class Success[+E](result: E) extends InferenceResult[E] {
    override def flatMap[F](f: (E) => InferenceResult[F]) = f(result)
  }

  case class Failure(message: String) extends InferenceResult[Nothing] {
    override def flatMap[F](f: (Nothing) => InferenceResult[F]): InferenceResult[F] = this
  }

  case class Context[X](types: List[Expr[X]]) {
    def prod(b: Expr[X]): Expr[X] = if (Nil == types) b else Suspend(Prod(types, b))

    @tailrec private def deduceAppl(x: Expr[X], y: List[Expr[X]]): InferenceResult[Expr[X]] = x match {
      case Suspend(Bound(n)) if n < types.length =>
        types(types.length - n - 1) match {
          case Suspend(Prod(u, v)) => combine(y, u, v)
          case other => Failure(s"product type required; $other is no product type")
        }
      case Suspend(Abst(u, v)) => combine(y, u, v)
      case Suspend(Appl(u, v)) => deduceAppl(u, v ++ y)
      case other => Failure(s"function required; $other is no function")
    }

    private def combine(y: List[Expr[X]], u: List[Expr[X]], v: Expr[X]): InferenceResult[Expr[X]] = {
      if (y.length > u.length) {
        val (h, t) = y splitAt u.length
        check(v, u zip h) flatMap (deduceAppl(_, t))
      } else {
        val (h, t) = u splitAt y.length
        check(v, h zip y) flatMap Context(types ++ t).deduce
      }
    }

    private def check(result: Expr[X], pairs: List[(Expr[X], Expr[X])]): InferenceResult[Expr[X]] = pairs match {
      case (t, e) :: tail => deduce(e) flatMap { u =>
        if (unifiable[X](List(t), List(u))) check(result, tail)
        else Failure(s"type mismatch: $e is no $t")
      }
      case Nil => Success(result)
    }

    private def deduceType(frees: List[Expr[X]], level: Int): InferenceResult[Expr[X]] = frees match {
      case Nil => Success(typ(level))
      case h :: t => deduce(h) match {
        case Success(Suspend(Type(n))) => deduceType(t, Math.max(level, n))
        case other => Failure(s"type required; $other is no type")
      }
    }

    @tailrec final def deduce(expr: Expr[X]): InferenceResult[Expr[X]] = expr match {
      case Suspend(Bound(n)) => types lift (types.length - n - 1) match {
        case Some(t) => Success(prod(t))
        case None => Failure(s"No binding for bound variable $n")
      }
      case Suspend(Abst(x, y)) => Context(types ++ x).deduce(y)
      case Suspend(Appl(x, y)) => deduceAppl(x, y)
      case Suspend(Type(n)) => Success(prod(typ(n + 1)))
      case Suspend(Prod(x, y)) => deduceType(y :: x, 0)
      case Return(v) => Success(Return(v))
    }
  }

}