package nl.woupiestek.andrej.typeclasses

import scala.language.higherKinds

trait Functor[F[_]] {
  self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {

  implicit class Ops[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(fa)(f)
  }

}

trait Apply[F[_]] extends Functor[F] {
  self =>
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]
}

trait Bind[F[_]] extends Apply[F] {
  self =>
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

}

trait Applicative[F[_]] extends Apply[F] {
  self =>
  def unit[A](a: A): F[A]
}

trait Monad[F[_]] extends Bind[F] with Applicative[F] {
  self =>
  override def map[A, B](fa: F[A])(f: A => B): F[B] = bind(fa)(f andThen unit)

  override def ap[A, B](fa: => F[A])(f: => F[(A) => B]): F[B] = bind(f)(map(fa))
}

object Bind {

  implicit class Ops[F[_], A](fa: F[A])(implicit F: Monad[F]) {
    def flatMap[B](f: A => F[B]): F[B] = F.bind(fa)(f)
  }

}
