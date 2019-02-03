package nl.woupiestek.equalizer.parsing

//todo: split into Empty/NonEmpty as well
final case class RuleT[F[_], I, O] private (
    matches: Boolean,
    head: F[O],
    tail: I => RuleT[F, I, O]
)

import scalaz._

object RuleT { parent =>

  implicit def isApplicativePlus[F[_], I](
      implicit F: ApplicativePlus[F]
  ): ApplicativePlus[({ type G[O] = RuleT[F, I, O] })#G] = {
    type G[O] = RuleT[F, I, O]
    new ApplicativePlus[G] {
      def point[A](a: => A): G[A] = RuleT(true, F.point(a), _ => empty)
      def ap[A, B](a: => G[A])(b: => G[A => B]): G[B] = RuleT(
        a.matches && b.matches,
        F.ap(a.head)(b.head),
        i =>
          if (a.matches) plus(ap(a.tail(i))(b), ap(a)(b.tail(i)))
          else ap(a.tail(i))(b)
      )
      def empty[A]: G[A] = new RuleT(false, F.empty, _ => empty)
      def plus[A](a: G[A], b: => G[A]): G[A] =
        RuleT(
          a.matches || b.matches,
          F.plus(a.head, b.head),
          i => plus(a.tail(i), b.tail(i))
        )
    }
  }

  implicit def isRule[F[_], I](
      implicit F: ApplicativePlus[F]
  ): Rule[({ type G[O] = RuleT[F, I, O] })#G, I] = {
    type G[O] = RuleT[F, I, O]
    new Rule[G, I] {
      def isApplicativePlus = parent.isApplicativePlus
      def lift[O](f: I => G[O]): G[O] = RuleT(false, F.empty, f)
    }
  }

}
