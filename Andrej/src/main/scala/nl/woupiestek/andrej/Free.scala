package nl.woupiestek.andrej

import scala.language.higherKinds

sealed trait Free[F[_], T] {

  def flatMap[U](f: T => Free[F, U]): Free[F, U]

  def map[U](f: T => U): Free[F, U] = flatMap(x => Free(f(x)))
}

object Free {

  case class Return[F[_], T](value: T) extends Free[F, T] {
    override def flatMap[U](f: (T) => Free[F, U]): Free[F, U] = f(value)
  }

  case class Bind[F[_], E, T](request: F[E], callback: E => Free[F, T]) extends Free[F, T] {
    override def flatMap[U](f: (T) => Free[F, U]): Free[F, U] = new Bind[F, E, U](request, x => callback(x).flatMap(f))
  }

  def apply[F[_], T](t: T): Free[F, T] = Return(t)

  def lift[F[_], T](ft: F[T]): Free[F, T] = Bind[F, T, T](ft, apply)
}
