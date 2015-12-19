package nl.woupiestek.andrej

import scala.language.higherKinds

object MonadUtils {

  trait Proof[F[_]] {
    def map[X, Y](f: X => Y)(x: F[X]): F[Y]

    def bind[X, Y](f: X => Y)(x: F[X]): F[Y]

    def ap[X, Y](f: F[X => Y])(x: F[X]): F[Y]
  }

  implicit class Ops[F[_], X](x: F[X])(implicit proof: Proof[F]) {
    def map[Y](f: X => Y): F[Y] = proof.map(f)(x)

    def flatMap[Y](f: X => F[Y]) = proof.bind(f)(x)

    def ap[Y](f: F[X => Y]) = proof.ap(f)(x)
  }

}
