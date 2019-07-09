package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

sealed abstract class DParser[-I] {
  final def d(i: I): DParser[I] = ???
}

object DParser {
  private final class Lazy[+I](f: => I) {
    lazy val value = f
  }

  private final case object Empty extends DParser[Any]
  private final case object Read extends DParser[Any]
  private final case class Sum[I](left: DParser[I], right: DParser[I])
      extends DParser[I]
  private final case class Write[I](i: I) extends DParser[I]
  private final case class Suspend[I](f: Lazy[DParser[I]]) extends DParser[I]

  private final case class FlatMap[I, J](dpi: DParser[I], dpj: I => DParser[J])
      extends DParser[J]
  
  def simplify[J](dp: DParser[J]): DParser[J] = {
    @tailrec def helper[X](
        head: DParser[X],
        tail: X => DParser[J]
    ): DParser[J] = head match {
      case Empty            => Empty
      case f: FlatMap[x, X] => helper[x](
        f.dpi, f.dpj(_).flatMap(tail))
      case w: Write[X] =>
        helper2(tail(w.i)) match {
          case FlatMap(x, y) => helper(x, y)
          case other         => other //not flatmap
        }
      case Suspend(f) => helper(f.value, tail)
      case _          => FlatMap(head, tail) // head is Read or Sum
    }

    def helper2(head: DParser[J]): DParser[J] = {
      var result = head
      while (head.isInstanceOf[Suspend[J]]) {
        result = result.asInstanceOf[Suspend[J]].f.value
      }
      result
    }

    helper2(dp) match {
      case FlatMap(x, y) => helper(x, y)
      case other         => other //not flatmap
    }
  }

  def suspend[I](dpi: => DParser[I]): DParser[I] = {
    Suspend(new Lazy(dpi))
  }

  def write[I](i: I): DParser[I] = Write(i)

  def empty[I]: DParser[I] = Empty

  implicit val isMonadPlus: MonadPlus[DParser] = new MonadPlus[DParser] {
    def bind[A, B](fa: DParser[A])(f: A => DParser[B]): DParser[B] =
      FlatMap(fa, f)
    def empty[A]: DParser[A] = Empty
    def plus[A](a: DParser[A], b: => DParser[A]): DParser[A] = Sum(a, b)
    def point[A](a: => A): DParser[A] = suspend(Write(a))
  }

  implicit def isMonoid[I]: Monoid[DParser[I]] = isMonadPlus.monoid

}
