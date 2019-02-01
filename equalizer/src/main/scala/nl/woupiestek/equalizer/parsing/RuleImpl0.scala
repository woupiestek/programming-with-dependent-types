package nl.woupiestek.equalizer.parsing

import scalaz._

final case class RuleImpl0[-I, +O](unfold: () => (I => RuleImpl0[I, O]) \/ (O, RuleImpl0[I, O]))

object RuleImpl0 {

  implicit def instance[I]: Rule[({type F[O] = RuleImpl0[I, O]})#F, I] = {
    type F[O] = RuleImpl0[I, O]
    new Rule[F, I] {
      override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
        RuleImpl0(() => fa.unfold() match {
          case -\/(a) => -\/((i: I) => ap(a(i))(f))
          case \/-((a0, a1)) => f.unfold() match {
            case -\/(b) => -\/((i: I) => ap(fa)(b(i)))
            case \/-((b0, b1)) => \/-((b0(a0), ap(a1)(b1)))
          }
        })

      override def point[A](a: => A): F[A] = RuleImpl0(() => \/-((a, empty)))

      override def empty[A]: F[A] = RuleImpl0(() => -\/(_ => empty))

      override def plus[A](a: F[A], b: => F[A]): F[A] =
        RuleImpl0(() => a.unfold().fold(
          c => b.unfold().bimap(
            d => i => plus(c(i), d(i)),
            { case (d, e) => (d, plus(a, e)) }),
          { case (c, d) => \/-((c, plus(d, b))) }))

      override def readIf(f: I => Boolean): F[I] =
        RuleImpl0(() => -\/((i: I) => if (f(i)) point(i) else empty))
    }
  }
}