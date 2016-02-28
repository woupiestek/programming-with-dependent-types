package nl.woupiestek.andrej

import scala.language.higherKinds

object MonadUtils {

  trait Functor[F[_]] {
    def map[X, Y](f: X => Y, fx: F[X]): F[Y]
  }

  trait Apply[F[_]] extends Functor[F] {
    def ap[X, Y](f: F[X => Y], fx: F[X]): F[Y]
  }

  trait Bind[F[_]] extends Apply[F] {
    def bind[X, Y](f: X => F[Y], fx: F[X]): F[Y]
  }

  trait Filterable[F[_]] {
    def filter[X](p: X => Boolean, fx: F[X]): F[X]
  }

  implicit class Ops[F[_], X](fx: F[X]) {
    def map[Y](f: X => Y)(implicit proof: Functor[F]): F[Y] = proof.map(f, fx)

    def flatMap[Y](f: X => F[Y])(implicit proof: Bind[F]): F[Y] = proof.bind(f, fx)

    def foreach(f: X => Any)(implicit proof: Functor[F]): Unit = proof.map(f, fx)

    def withFilter(f: X => Boolean)(implicit proof: Filterable[F]): WithFilter[F, X] = new WithFilter[F, X](f, fx, proof)
  }

  private class WithFilter[F[_], X](p: X => Boolean, fx: F[X], filterable: Filterable[F]) {
    private lazy val filtered = filterable.filter(p, fx)

    def map[Y](f: X => Y)(implicit proof: Functor[F]): F[Y] = proof.map(f, filtered)

    def flatMap[Y](f: X => F[Y])(implicit proof: Bind[F]): F[Y] = proof.bind(f, filtered)

    def foreach(f: X => Any)(implicit proof: Functor[F]): Unit = proof.map(f, filtered)

    def withFilter(f: X => Boolean): WithFilter[F, X] = new WithFilter[F, X](x => p(x) && f(x), fx, filterable)
  }

}

