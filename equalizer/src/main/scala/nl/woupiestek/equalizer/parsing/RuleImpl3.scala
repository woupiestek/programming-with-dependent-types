package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
trait RuleImpl3[I, O] {

  def foldMap[P: Monoid](f: O => P): P

  def next: I => RuleImpl3[I, O]
}

object RuleImpl3 {

  implicit def isApplicativePlus[I]
      : ApplicativePlus[({ type F[O] = RuleImpl3[I, O] })#F] = {
    type F[O] = RuleImpl3[I, O]
    new ApplicativePlus[F] {
      override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = new F[B] {
        def foldMap[P](g: B => P)(implicit P: scalaz.Monoid[P]): P =
          (for {
            a0 <- fa.foldMap(a => IList(a))
            b0 <- f.foldMap(b => IList(b))
          } yield b0(a0)).foldMap(g)

        def next: I => RuleImpl3[I, B] =
          (i: I) =>
            plus(ap(fa.next(i))(f), ap(new RuleImpl3[I, A] {
              def foldMap[P](h: A => P)(implicit P: scalaz.Monoid[P]) =
                fa.foldMap(h)
              def next = (_ => empty)
            })(f.next(i)))
      }

      override def empty[A]: F[A] = new F[A] {
        def foldMap[P](g: A => P)(implicit P: scalaz.Monoid[P]): P = P.zero
        def next: I => RuleImpl3[I, A] = (_ => empty)
      }

      override def plus[A](a: F[A], b: => F[A]): F[A] = new F[A] {
        def foldMap[P](g: A => P)(implicit P: scalaz.Monoid[P]): P =
          P.append(a.foldMap(g), b.foldMap(g))
        def next: I => RuleImpl3[I, A] =
          i => plus(a.next(i), b.next(i))
      }
      override def point[A](a: => A): F[A] = new F[A] {
        def foldMap[P](g: A => P)(implicit P: scalaz.Monoid[P]): P = g(a)
        def next: I => RuleImpl3[I, A] = (_ => empty)
      }
    }
  }

}
