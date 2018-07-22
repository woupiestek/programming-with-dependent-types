package nl.woupiestek.andrej.free

import nl.woupiestek.andrej.free.TagFree.pure
import nl.woupiestek.andrej.typeclasses.Monad

import scala.collection.generic.CanBuildFrom
import scala.language.{higherKinds, reflectiveCalls}

trait TagFree[F[_], X] {
  self =>
  def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[X]

  def flatMap[Y](f: X => TagFree[F, Y]): TagFree[F, Y] = new TagFree[F, Y] {
    override def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[Y] = M.bind[X, Y](self.eval(fm))(f(_).eval(fm))
  }

  def map[Y](f: X => Y): TagFree[F, Y] = flatMap(x => pure(f(x)))

}

trait ==>[F[_], G[_]] {
  def apply[X](fx: F[X]): G[X]
}

object TagFree {

  def pure[F[_], X](x: X): TagFree[F, X] = new TagFree[F, X] {
    override def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[X] = M.unit(x)
  }

  def execute[F[_], X](fx: F[X]): TagFree[F, X] = new TagFree[F, X] {
    override def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[X] = fm(fx)
  }

  def join[F[_], X](fx: F[TagFree[F, X]]): TagFree[F, X] = new TagFree[F, X] {
    override def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[X] = M.bind(fm(fx))(_.eval(fm))
  }

  implicit def instanceFor[F[_]]: Monad[({type TF[X] = TagFree[F, X]})#TF] =
    new Monad[({type TF[X] = TagFree[F, X]})#TF] {
      override def unit[A](a: A): TagFree[F, A] = pure(a)

      override def bind[A, B](fa: TagFree[F, A])(f: A => TagFree[F, B]): TagFree[F, B] = fa.flatMap(f)
    }

  implicit class TraversableOps[T[X] <: TraversableOnce[X], A](in: T[A]) {
    def traverse[F[_], B](f: A => TagFree[F, B])(
      implicit cbf: CanBuildFrom[T[A], B, T[B]]): TagFree[F, T[B]] = new TagFree[F, T[B]] {
      override def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[T[B]] =
        M.map(in.foldLeft(M.unit(cbf(in)))((fr, a) => M.bind(fr)(builder => M.map(f(a).eval(fm))(builder += _))))(_.result())
    }

    def foldLeftM[F[_], B](b: B)(f: (B, A) => TagFree[F, B]): TagFree[F, B] = new TagFree[F, B] {
      override def eval[M[_]](fm: F ==> M)(implicit M: Monad[M]): M[B] =
        in.foldLeft(M.unit(b))((b, a) => M.bind(b)(f(_, a).eval(fm)))
    }
  }

}
