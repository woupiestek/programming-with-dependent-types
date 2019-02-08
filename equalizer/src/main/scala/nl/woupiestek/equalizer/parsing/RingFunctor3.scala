package nl.woupiestek.equalizer.parsing

import scalaz._
import RingFunctor3._
import scala.annotation.tailrec

trait RingFunctor3[A] { base =>
  def fr[B](b: => B)(f: (A, => B) => B): B

  def append(a: A) = new RingFunctor3[A] {
    def fr[B](b: => B)(f: (A, => B) => B): B =
      base.fr(f(a, b))(f)
  }

  def unfold: Maybe[(A, RingFunctor3[A])] = {
    def p(
        x: A,
        y: => Maybe[(A, RingFunctor3[A])]
    ): Maybe[(A, RingFunctor3[A])] =
      Maybe.just(
        (
          x,
          y.cata(
            (z: (A, RingFunctor3[A])) => z._2.append(z._1),
            isApplicativePlus.empty[A]
          )
        )
      )
    fr(Maybe.empty[(A, RingFunctor3[A])])(p)
  }
}

object RingFunctor3 {
  type RF[A] = RingFunctor3[A]

  implicit val isApplicativePlus: ApplicativePlus[RF] =
    new ApplicativePlus[RF] {
      def point[A](a: => A): RF[A] = new RF[A] {
        def fr[B](b: => B)(f: (A, => B) => B): B = f(a, b)
      }

      def ap[A, B](fa: => RF[A])(f: => RF[A => B]): RF[B] = new RF[B] {
        def fr[C](b: => C)(g: (B, => C) => C): C = {
          //fa.fr(b)((a, c) => f.fr(c)((d, e) => g(d(a), e)))
          @tailrec def p(ga: => RF[A], b: C): C = ga.unfold match {
            case Maybe.Empty()      => b
            case Maybe.Just((a, c)) => p(c, f.fr(b)((d, e) => g(d(a), e)))
          }
          p(fa, b)
        }
      }

      def plus[A](a: RF[A], b: => RF[A]): RF[A] = new RF[A] {
        def fr[B](c: => B)(f: (A, => B) => B): B =
          a.fr(b.fr(c)(f))(f)
      }

      def empty[A]: RF[A] = new RF[A] {
        def fr[B](c: => B)(f: (A, => B) => B): B = c
      }

    }

  implicit def isFoldable: Foldable[RF] = new Foldable[RF] {
    def foldMap[A, B](fa: RF[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B =
      foldRight(fa, F.zero)((a, b) => F.append(f(a), b))

    def foldRight[A, B](fa: RF[A], z: => B)(f: (A, => B) => B): B = fa.fr(z)(f)
  }

}
