package nl.woupiestek.andrej.free

import nl.woupiestek.andrej.free.Tail.{Break, Continue}
import nl.woupiestek.andrej.typeclasses.{Applicative, Bind, Functor}

sealed trait Tail[F[_], X, Y]

object Tail {

  case class Break[F[_], X, Y](y: Y) extends Tail[F, X, Y]

  case class Continue[F[_], X, Y](fx: F[X]) extends Tail[F, X, Y]

}

case class WFree[F[_], X](tail: Tail[F, WFree[F, X], X])

object WFree {

  type T[F[_]] = Bind[F] with While[F]

  implicit def instance[F[_]](implicit F: Functor[F]): T[({type G[X] = WFree[F, X]})#G] =
    new T[({type G[X] = WFree[F, X]})#G] {
      override def `while`[X, Y](f: (X) => Tail[({
        type G[X0] = WFree[F, X0]
      })#G, X, Y]): (X) => WFree[F, Y] = x => f(x) match {
        case Break(y) => WFree(Break(y))
        case Continue(fy) => bind(fy)(`while`(f))
      }

      override def bind[A, B](fa: WFree[F, A])(f: (A) => WFree[F, B]): WFree[F, B] =
        fa.tail match {
          case Break(a) => f(a)
          case Continue(fa2) => WFree(Continue(F.map(fa2)(bind(_)(f))))
        }
    }

  def kata[F[_], X](wfx: WFree[F, X])(implicit F: While[F]): F[X] = F.`while`[WFree[F, X], X]((x: WFree[F, X]) => x.tail)(wfx)

}


trait While[F[_]] extends Applicative[F] {
  self =>
  def `while`[X, Y](f: X => Tail[F, X, Y]): X => F[Y]

  def unit[X](x: X): F[X] = `while`(Break[F, X, X](_))(x)
}

