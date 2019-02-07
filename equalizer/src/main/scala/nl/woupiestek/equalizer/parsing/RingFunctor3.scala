package nl.woupiestek.equalizer.parsing

import scalaz._

trait RingFunctor3[A] {
  def fr[B](b: => B)(f: (A, => B) => B): B
}

object RingFunctor3 {
  type RF[A] = RingFunctor3[A]

  implicit def isApplicativePlus: ApplicativePlus[RF] =
    new ApplicativePlus[RF] {
      def point[A](a: => A): RF[A] = new RF[A] {
        def fr[B](b: => B)(f: (A, => B) => B): B = f(a, b)
      }

      def ap[A, B](fa: => RF[A])(f: => RF[A => B]): RF[B] = new RF[B] {
        def fr[C](b: => C)(g: (B, => C) => C): C =
          fa.fr(b)((a, c) => f.fr(c)((d, e) => g(d(a), e)))
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
