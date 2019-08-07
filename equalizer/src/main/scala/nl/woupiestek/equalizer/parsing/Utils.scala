package nl.woupiestek.equalizer.parsing

import scalaz._

object Utils {

  implicit def monad[F[_]](
      implicit A: ApplicativePlus[F],
      F: Foldable[F]
  ): Monad[F] = {
    implicit def monoid[B]: Monoid[F[B]] = A.monoid
    new Monad[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = F.foldMap(fa)(f)
      def point[A](a: => A): F[A] = A.point(a)
    }
  }

}
