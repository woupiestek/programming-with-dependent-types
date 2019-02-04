package nl.woupiestek.equalizer.parsing

final case class RuleT[F[_], I, O] private (
    matches: F[O],
    derive: I => RuleT[F, I, O]
)

import scalaz._

object RuleT { parent =>
  implicit def isApplicativePlus[F[_], I](
      implicit F: ApplicativePlus[F]
  ): ApplicativePlus[({ type G[O] = RuleT[F, I, O] })#G] = {
    type G[O] = RuleT[F, I, O]
    new ApplicativePlus[G] {
      def point[A](a: => A): G[A] = RuleT(F.point(a), _ => empty)
      def ap[A, B](a: => G[A])(b: => G[A => B]): G[B] = RuleT(
        F.ap(a.matches)(b.matches),
        i =>
          plus(
            ap(a.derive(i))(b),
            ap(RuleT(a.matches, _ => empty[A]))(b.derive(i))
          )
      )
      def empty[A]: G[A] = RuleT(F.empty, _ => empty)
      def plus[A](a: G[A], b: => G[A]): G[A] =
        RuleT(
          F.plus(a.matches, b.matches),
          i => plus(a.derive(i), b.derive(i))
        )
    }
  }

  implicit def isRule[F[_], I](
      implicit F: ApplicativePlus[F]
  ): Rule[({ type G[O] = RuleT[F, I, O] })#G, I] = {
    type G[O] = RuleT[F, I, O]
    new Rule[G, I] {
      def isApplicativePlus = parent.isApplicativePlus
      def lift[O](f: I => G[O]): G[O] = RuleT(F.empty, f)
    }
  }

}
