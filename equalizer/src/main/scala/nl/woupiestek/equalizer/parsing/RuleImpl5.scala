package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed trait RuleImpl5[I, O] {
  def next(i: I): RuleImpl5[I, O]
  def fold[R[_]](implicit R: ApplicativePlus[R]): R[O]
}

object RuleImpl5 {

  implicit def instance[I]: Rule[({ type F[O] = RuleImpl5[I, O] })#F, I] = {
    type F[O] = RuleImpl5[I, O]
    new Rule[F, I] {
      override def lift[O](f: I => F[O]): F[O] = new F[O] {
        def next(i: I): RuleImpl5[I, O] = f(i)
        def fold[R[_]](implicit R: ApplicativePlus[R]): R[O] = R.empty[O]
      }
      override implicit val isApplicativePlus: ApplicativePlus[F] =
        new ApplicativePlus[F] {
          override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = new F[B] {
            private lazy val f0 = f
            def next(i: I): RuleImpl5[I, B] =
              plus(ap(fa.next(i))(f0), ap(fa)(f0.next(i)))
            def fold[R[_]](implicit R: ApplicativePlus[R]): R[B] =
              fa.fold[R] <*> f.fold[R]
          }

          override def empty[A]: F[A] = new F[A] {
            def next(i: I): RuleImpl5[I, A] = empty
            def fold[R[_]](implicit R: ApplicativePlus[R]): R[A] =
              R.empty[A]
          }

          override def plus[A](a: F[A], b: => F[A]): F[A] = new F[A] {
            private lazy val b0 = b
            def next(i: I): RuleImpl5[I, A] = plus(a.next(i), b0.next(i))

            def fold[R[_]](implicit R: ApplicativePlus[R]): R[A] =
              a.fold[R] <+> b0.fold[R]
          }

          override def point[A](a: => A): F[A] = new F[A] {
            def next(i: I): RuleImpl5[I, A] = empty
            def fold[R[_]](implicit R: ApplicativePlus[R]): R[A] =
              R.point(a)
          }
        }
    }
  }
}
