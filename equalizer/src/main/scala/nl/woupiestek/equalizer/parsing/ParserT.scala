package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import ParserT._
import scala.collection.mutable

sealed abstract class ParserT[F[_], I, O] {
  def value: F[O]
  def derive(i: I): ParserT[F, I, O]

  def parse[G[_]: Foldable](input: G[I]): F[O] = {
    def dm = memoized((p: ParserT[F, I, O]) => memoized((i: I) => p.derive(i)))

    input.foldLeft(this)(dm(_)(_)).value
  }

}
object ParserT {

  implicit def isApplicativePlus[F[_], I](
      implicit F: ApplicativePlus[F]
  ): ApplicativePlus[({ type G[O] = ParserT[F, I, O] })#G] = {
    type G[O] = ParserT[F, I, O]
    new ApplicativePlus[G] {
      def empty[A]: G[A] = new G[A] { self =>
        def value = F.empty
        def derive(i: I) = self
      }

      def point[A](a: => A): G[A] = new G[A] {
        def value = F.point(a)
        def derive(i: I) = empty[A]
      }

      def ap[A, B](fa: => G[A])(f: => G[A => B]): G[B] = new G[B] {
        def value = F.ap(fa.value)(f.value)

        def derive(i: I) =
          plus(ap(fa.derive(i))(f), ap(new G[A] {
            def value = fa.value
            def derive(j: I) = empty[A]
          })(f.derive(i)))
      }

      def plus[A](a: G[A], b: => G[A]): G[A] = new G[A] {
        def value = F.plus(a.value, b.value)
        def derive(i: I) = plus(a.derive(i), b.derive(i))
      }
    }
  }

  implicit class applicativePlusOps[R[_], O](rule: R[O])(
      implicit R: ApplicativePlus[R]
  ) {
    def nel: R[NonEmptyList[O]] = (rule |@| rule.list)(NonEmptyList.nel)

    def list: R[IList[O]] = rule.nel.map(_.list) <+> IList.empty[O].point[R]

    def maybe: R[Maybe[O]] =
      rule.map(_.point[Maybe]) <+> Maybe.empty[O].point[R]
  }

  case class If[I](f: I => Boolean) extends AnyVal {
    def read[F[_]](implicit F: ApplicativePlus[F]): ParserT[F, I, I] =
      new ParserT[F, I, I] {
        def value = ApplicativePlus[F].empty
        def derive(i: I) =
          if (f(i)) isApplicativePlus[F, I].point(i)
          else isApplicativePlus[F, I].empty
      }

    def scanMap[F[_],B](
        f: I => B
    )(implicit B: scalaz.Monoid[B], F: ApplicativePlus[F]): ParserT[F, I, B] =
      scanRight(B.zero)((i, b) => B.append(f(i), b))

    def scanRight[F[_],B](
        z: => B
    )(f: (I, => B) => B)(implicit F: ApplicativePlus[F]): ParserT[F, I, B] =
      (read[F] |@| scanRight(z)(f))(f(_, _)) <+> isApplicativePlus[F, I]
        .point(z)
  }

  private def memoized[A, B](f: A => B): A => B = {
    val memo = mutable.Map.empty[A, B]

    (a: A) =>
      memo.getOrElse(a, {
        val b = f(a)
        memo += (a -> b)
        b
      })
  }
}
