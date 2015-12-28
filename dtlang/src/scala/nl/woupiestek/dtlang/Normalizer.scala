package nl.woupiestek.dtlang

import scala.language.postfixOps

object Normalizer {

  sealed trait Expr {
    final def eval(value: Expr): Expr = eval(value, 0)

    protected def eval(value: Expr, index: Int): Expr
  }

  case class Vari(depth: Int) extends Expr {
    def eval(value: Expr, index: Int) =
      if (depth < index) this
      else if (depth == index) value
      else Vari(depth - 1)
  }

  case class Appl(operator: Expr, operand: Expr) extends Expr {
    override def eval(value: Expr, index: Int): Expr = Appl(operator.eval(value, index), operand.eval(value, index))
  }

  case class Abst(sort: Expr, body: Expr) extends Expr {
    override def eval(value: Expr, index: Int): Expr = Abst(sort.eval(value, index), body.eval(value, index))
  }

  case class Prod(sort: Expr, body: Expr) extends Expr {
    override def eval(value: Expr, index: Int): Expr = Prod(sort.eval(value, index), body.eval(value, index))
  }

  case class Univ(level: Int) extends Expr {
    override def eval(value: Expr, index: Int): Expr = this
  }

  class Context(sorts: List[Expr]) {

    def normalFormOf: Expr => Option[Expr] = Memoized {
      reduce => {
        case Vari(i) if i < sorts.length =>
          Some(Vari(i))
        case Appl(x, y) => for {
          Abst(a, b) <- reduce(x)
          c <- typeOf(y) if c == a
          d <- reduce(y)
        } yield b.eval(d)
        case Abst(x, y) => for {
          a <- reduce(x)
          b <- reduce(y)
        } yield Abst(a, b)
        case Prod(x, y) => for {
          a <- reduce(x)
          b <- reduce(y)
        } yield Abst(a, b)
        case Univ(i) => Some(Univ(i))
        case _ => None
      }
    }

    def typeOf: Expr => Option[Expr] = Memoized {
      typeOf => {
        case Vari(i) => sorts lift i
        case Appl(x, y) => for {
          Prod(a, b) <- typeOf(x)
          c <- typeOf(y) if c == a
          d <- normalFormOf(y)
          e <- normalFormOf(b.eval(d))
        } yield e
        case Abst(x, y) => for {
          c <- normalFormOf(x)
          b <- new Context(c::sorts).typeOf(y)
        } yield Prod(c, b)
        case Prod(x, y) => for {
          Univ(i) <- typeOf(x)
          tx <- normalFormOf(x)
          Univ(j) <- typeOf(y)
        } yield Prod(tx, Univ(Math.max(i, j)))
        case Univ(i) => Some(Univ(i + 1))
      }
    }
  }


}
