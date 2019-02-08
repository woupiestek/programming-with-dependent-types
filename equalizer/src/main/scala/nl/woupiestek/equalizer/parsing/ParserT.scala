package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import ParserT._
import scala.collection.mutable

sealed abstract class ParserT[-I, +O] {
  def value[F[_]: ApplicativePlus, P >: O]: F[P]
  def derive(i: I): ParserT[I, O]

  def parse[F[_]: ApplicativePlus, G[_]: Foldable, J <: I, P >: O](
      input: G[J]
  ): F[P] = {
    def dm = memoized((p: ParserT[I, O]) => memoized((i: I) => p.derive(i)))

    input.foldLeft(this)(dm(_)(_)).value[F, P]
  }

}

object ParserT {
  private case object Empty extends ParserT[Any, Nothing] {
    def value[F[_]: ApplicativePlus, B >: Nothing] = ApplicativePlus[F].empty[B]
    def derive(i: Any) = this
  }

  implicit def isApplicativePlus[I]
      : ApplicativePlus[({ type G[O] = ParserT[I, O] })#G] = {
    type G[O] = ParserT[I, O]
    new ApplicativePlus[G] {
      def empty[A]: G[A] = Empty

      def point[A](a: => A): G[A] = new ParserT[I, A] {
        def value[F[_]: ApplicativePlus, B >: A] = ApplicativePlus[F].point(a)
        def derive(i: I) = Empty
      }

      def ap[A, B](fa: => G[A])(f: => G[A => B]): G[B] =
        new G[B] {
          def value[F[_]: ApplicativePlus, C >: B] =
            ApplicativePlus[F].ap(fa.value[F, A])(f.value[F, A => C])

          def derive(i: I) =
            plus(ap(fa.derive(i))(f), ap(new G[A] {
              def value[F[_]: ApplicativePlus, C >: A] = fa.value[F, C]
              def derive(j: I) = Empty
            })(f.derive(i)))
        }

      def plus[A](a: G[A], b: => G[A]): G[A] =
        if (a == Empty) b
        else
          new G[A] {
            def value[F[_]: ApplicativePlus, B >: A] =
              ApplicativePlus[F].plus(a.value[F, B], b.value[F, B])
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
    def one: ParserT[I, I] =
      new ParserT[I, I] {
        def value[F[_]: ApplicativePlus, J >: I] = ApplicativePlus[F].empty
        def derive(i: I) =
          if (f(i)) isApplicativePlus[I].point(i)
          else Empty
      }

    def scanMap[B](f: I => B)(implicit B: scalaz.Monoid[B]): ParserT[I, B] =
      scanRight(B.zero)((i, b) => B.append(f(i), b))

    def scanRight[B](z: => B)(f: (I, => B) => B): ParserT[I, B] =
      (one |@| scanRight(z)(f))(f(_, _)) <+> isApplicativePlus[I].point(z)
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
