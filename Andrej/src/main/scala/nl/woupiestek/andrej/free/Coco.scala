package nl.woupiestek.andrej.free

import nl.woupiestek.andrej.free.Coco.Extend

abstract class Coco[F[_], A](val extract: A) {

  def run[B](f: Coco[F, A] => B): F[B]

  def extend[B](f: Coco[F, A] => B): Coco[F, B] = new Extend[F, A, B](this, f)

}

object Coco {

  private class Extend[F[_], A0, A](coco: Coco[F, A0], f0: Coco[F, A0] => A) extends Coco[F, A](f0(coco)) {
    override def run[B](f: (Coco[F, A]) => B): F[B] = coco.run(a0 => f(a0.extend(f0)))
  }

  def ana[F[_], A](fa: F[A])(implicit F: Comonad[F]): Coco[F, A] = new Ana[F, A](fa)

  private class Ana[F[_], A](fa: F[A])(implicit F: Comonad[F]) extends Coco[F, A](F.extract(fa)) {
    override def run[B](f: (Coco[F, A]) => B): F[B] = F.extend(fa)(f compose ana[F, A])
  }

}

trait Comonad[C[_]] {
  def extract[T](ct: C[T]): T

  def extend[T, U](ct: C[T])(f: C[T] => U): C[U]

  def map[T, U](ct: C[T])(f: T => U): C[U] = extend(ct)(f compose extract)
}