package nl.woupiestek.andrej.free

import nl.woupiestek.andrej.typeclasses.{ Bind, Monad }
import scala.language.higherKinds

import scalaz.{ -\/, \/, \/- }

sealed trait RFB[F[_], A] {
  def resume(implicit F: Monad[F]): F[RFB[F, A] \/ A]

  def flatMap[B](f: (A) => RFB[F, B]): RFB[F, B]
}

object RFB {

  case class RUnit[F[_], A](a: A) extends RFB[F, A] {
    override def resume(implicit F: Monad[F]): F[\/[RFB[F, A], A]] = F.unit(\/-(a))

    override def flatMap[B](f: (A) => RFB[F, B]): RFB[F, B] = f(a)
  }

  case class RBind[F[_], A0, A](fa: F[A0], g: A0 => RFB[F, A]) extends RFB[F, A] {
    override def resume(implicit F: Monad[F]): F[\/[RFB[F, A], A]] = F.map(fa)(a => -\/(g(a)))

    override def flatMap[B](f: (A) => RFB[F, B]): RFB[F, B] = copy(g = g(_: A0).flatMap(f))
  }

  def kata[F[_], A](implicit F: MonadRec[F]): RFB[F, A] => F[A] = F.tailRec[RFB[F, A], A](_.resume)

  implicit def instance[F[_]]: MonadRec[({ type G[A] = RFB[F, A] })#G] = new MonadRec[({ type G[A] = RFB[F, A] })#G] {

    override def tailRec[A, B](f: (A) => RFB[F, \/[A, B]]): (A) => RFB[F, B] =
      f(_: A).flatMap {
        case \/-(b) => unit(b)
        case -\/(a) => tailRec(f)(a)
      }

    override def bind[A, B](fa: RFB[F, A])(f: (A) => RFB[F, B]): RFB[F, B] = fa.flatMap(f)

    override def unit[A](a: A): RFB[F, A] = RUnit(a)
  }

}

trait BindRec[F[_]] extends Bind[F] {
  self =>
  def tailRec[A, B](f: A => F[A \/ B]): A => F[B]
}

trait MonadRec[F[_]] extends BindRec[F] with Monad[F]

