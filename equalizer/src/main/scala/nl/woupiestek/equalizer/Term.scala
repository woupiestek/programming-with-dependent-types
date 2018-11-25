package nl.woupiestek.equalizer

import scala.language.higherKinds

trait Term[F[_], T] {

  def variable(name: String): F[T]

  def let(name: String, value: T, body: T): F[T]

  def application(operator: T, operand: T): F[T]

  def abstraction(name: String, body: T): F[T]

  def reflection(left: T, right: T): F[T]

  def guard(left: T, right: T, body: T): F[T]
}

trait Type[F[_], Ty, Te] {

  def variable(index: Int): F[Ty]

  def let(name: String, value: Te, body: Ty): F[Ty]

  def product(name: String, dom: Ty, cod: Ty): F[Ty]

  def path(left: Te, right: Te): F[Ty]
}
