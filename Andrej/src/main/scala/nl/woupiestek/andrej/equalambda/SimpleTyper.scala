package nl.woupiestek.andrej.equalambda

import nl.woupiestek.andrej.free.TagFree
import nl.woupiestek.andrej.free.TagFree._

import scala.annotation.tailrec
import scala.language.higherKinds

trait SimpleTyper {

  //we will need intermediate structures at least in order to carry out comparisons.

  //something like this still seems inevitable
  case class Arrow(dom: List[Arrow], cod: Int) {
    def ->:(arrow: Arrow): Arrow = copy(dom = arrow :: dom)
  }

  //no elimination (yet)
  @tailrec final def solve(
      equations: List[(Arrow, Arrow)],
      values: Map[Int, Arrow]
  ): Map[Int, Arrow] = equations match {
    case Nil => values
    case (Arrow(a, b), Arrow(c, d)) :: t
        if a.length == c.length =>
      val tuples = a.zip(c) ++ t
      if (b == d) solve(tuples, values)
      else
        (values.get(b), values.get(d)) match {
          case (Some(e), Some(f)) =>
            solve((e, f) :: tuples, values)
          case (Some(e), None) =>
            solve(tuples, values + (d -> e))
          case (None, Some(f)) =>
            solve(tuples, values + (d -> f))
          case (None, None) =>
            solve(tuples, values + (d -> Arrow(Nil, b)))
        }
    case (Arrow(a, b), Arrow(c, d)) :: t
        if a.length < c.length =>
      val arrow = Arrow(c.drop(a.length), d)
      val tuples = a.zip(c) ++ t
      values.get(b) match {
        case Some(e) => solve((e, arrow) :: tuples, values)
        case None    => solve(tuples, values + (b -> arrow))
      }
    case (Arrow(c, d), Arrow(a, b)) :: t
        if a.length < c.length =>
      val arrow = Arrow(c.drop(a.length), d)
      val tuples = a.zip(c) ++ t
      values.get(b) match {
        case Some(e) => solve((e, arrow) :: tuples, values)
        case None    => solve(tuples, values + (b -> arrow))
      }
  }

  //think about the actions of the typer though...
  trait Action[A[_]] {
    def generate(): A[Arrow]

    def register(varName: String): A[Arrow]

    def unify(a: Arrow, b: Arrow): A[Unit]
  }

  class instance[A[_]](implicit A: Action[A])
      extends Term[TagFree[A, Arrow]] {
    override def identifier(name: String): TagFree[A, Arrow] =
      execute(A.register(name))

    override def where(
        term: TagFree[A, Arrow],
        substitution: Map[String, TagFree[A, Arrow]]
    ): TagFree[A, Arrow] = {
      for {
        _ <- substitution.toList.traverse {
          case (key, value) =>
            for {
              t <- execute(A.register(key))
              u <- value
              _ <- execute(A.unify(t, u)) //this sucks...
            } yield ()
        }
        v <- term
      } yield v
    }

    override def application(
        operator: TagFree[A, Arrow],
        operands: List[TagFree[A, Arrow]]
    ): TagFree[A, Arrow] =
      for {
        z <- execute(A.generate())
        y <- operands.foldLeftM(z) { (dom, cod) =>
          cod.map(dom ->: _)
        }
        x <- operator
        _ <- execute(A.unify(x, y))
      } yield z

    override def abstraction(
        arg: String,
        body: TagFree[A, Arrow]
    ): TagFree[A, Arrow] =
      for {
        x <- execute(A.register(arg))
        y <- body
        z <- execute(A.generate())
        _ <- execute(A.unify(z, x ->: y))
      } yield z
  }

}
