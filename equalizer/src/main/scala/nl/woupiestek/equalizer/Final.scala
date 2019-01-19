package nl.woupiestek.equalizer

import scalaz.Functor
import scalaz.Scalaz._

import scala.language.higherKinds

final case class Final[F[_]](unfold: () => F[Final[F]])

object Final {
  def cons[F[_]](ff: => F[Final[F]]) = Final(() => ff)

  def ana[F[_] : Functor, A](f: A => F[A])(a: A): Final[F] =
    cons(f(a).map(ana(f)))
}

