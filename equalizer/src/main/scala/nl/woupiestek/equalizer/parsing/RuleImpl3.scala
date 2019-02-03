package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
trait RuleImpl3[I, O] {

  def foldMap[P: Monoid](f: O => P): P

  def next: Maybe[I => RuleImpl3[I, O]]
}

object RuleImpl3 {

  implicit def instance[I]: Rule[({ type F[O] = RuleImpl3[I, O] })#F, I] = {
    type F[O] = RuleImpl3[I, O]
    new Rule[F, I] {
      override def lift[O](f: I => F[O]): F[O] = new F[O] {
        def foldMap[P](g: O => P)(implicit P: scalaz.Monoid[P]): P = P.zero
        def next: Maybe[I => RuleImpl3[I, O]] = Maybe.just(f)
      }
      override implicit val isApplicativePlus: ApplicativePlus[F] =
        new ApplicativePlus[F] {
          override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = new F[B] {
            def foldMap[P](g: B => P)(implicit P: scalaz.Monoid[P]): P =
              (for {
                a0 <- fa.foldMap(a => IList(a))
                b0 <- f.foldMap(b => IList(b))
              } yield b0(a0)).foldMap(g)

            def next: Maybe[I => RuleImpl3[I, B]] =
              fa.next
                .map(ga => (i: I) => ap(ga(i))(f))
                .orElse(f.next.map(g => (i: I) => ap(fa)(g(i))))
          }

          override def empty[A]: F[A] = new F[A] {
            def foldMap[P](g: A => P)(implicit P: scalaz.Monoid[P]): P = P.zero
            def next: Maybe[I => RuleImpl3[I, A]] = Maybe.empty
          }

          override def plus[A](a: F[A], b: => F[A]): F[A] = new F[A] {
            def foldMap[P](g: A => P)(implicit P: scalaz.Monoid[P]): P =
              P.append(a.foldMap(g), b.foldMap(g))
            def next: Maybe[I => RuleImpl3[I, A]] =
              (a.next, b.next) match {
                case (Maybe.Just(c), Maybe.Just(d)) =>
                  Maybe.just((i: I) => plus(c(i), d(i)))
                case (Maybe.Just(c), Maybe.Empty()) => Maybe.just(c)
                case (Maybe.Empty(), Maybe.Just(c)) => Maybe.just(c)
                case (Maybe.Empty(), Maybe.Empty()) => Maybe.empty
              }
          }
          override def point[A](a: => A): F[A] = new F[A] {
            def foldMap[P](g: A => P)(implicit P: scalaz.Monoid[P]): P = g(a)
            def next: Maybe[I => RuleImpl3[I, A]] = Maybe.empty
          }
        }
    }
  }
}
