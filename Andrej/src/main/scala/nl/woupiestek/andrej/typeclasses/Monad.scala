package nl.woupiestek.andrej.typeclasses

import scala.language.higherKinds

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad {
  implicit class Ops[F[_], A](fa: F[A])(implicit F: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = F.bind(fa)(f)
    def map[B](f: A => B): F[B] = F.bind(fa)(f andThen F.unit)
  }
}
