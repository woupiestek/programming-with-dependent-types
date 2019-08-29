package nl.woupiestek.andrej.typeclasses

import scala.language.higherKinds

object StateT {

  type StateT[F[_], S, T] = S => F[(T, S)]

  implicit class Transformer[F[_], S](F: Monad[F])
      extends Monad[({ type G[T] = StateT[F, S, T] })#G] {

    override def unit[A](a: A): StateT[F, S, A] =
      (s: S) => F.unit((a, s))

    override def bind[A, B](
        fa: StateT[F, S, A]
    )(f: (A) => StateT[F, S, B]): StateT[F, S, B] =
      (s: S) => F.bind(fa(s)) { case (a, t) => f(a)(t) }
  }

}
