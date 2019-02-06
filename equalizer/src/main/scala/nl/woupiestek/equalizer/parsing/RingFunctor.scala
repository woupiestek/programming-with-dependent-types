package nl.woupiestek.equalizer.parsing

import scalaz._

sealed abstract class RingFunctor[+A]

object RingFunctor {
  type RF[A] = RingFunctor[A]

  private final case object Empty extends RF[Nothing]
  private sealed abstract class NERF[A] extends RF[A]
  private final case class Point[A](a: Need[A]) extends NERF[A]
  private final case class Plus[A](a: NERF[A], b: NERF[A]) extends NERF[A]
  private final case class Ap[A, B](a: NERF[A], b: NERF[A => B]) extends NERF[B]

  implicit val isApplicativePlus: ApplicativePlus[RF] =
    new ApplicativePlus[RF] {
      def empty[A]: RF[A] = Empty
      def point[A](a: => A): RF[A] = Point(Need(a))
      def plus[A](a: RF[A], b: => RF[A]): RF[A] = (a, b) match {
        case (Empty, b0)                => b0
        case (a0, Empty)                => a0
        case (a0: NERF[A], b0: NERF[A]) => Plus(a0, b0)
      }

      def ap[A, B](a: => RF[A])(b: => RF[A => B]): RF[B] = (a, b) match {
        case (Empty, _)                    => Empty
        case (_, Empty)                    => Empty
        case (c: NERF[A], d: NERF[A => B]) => Ap(c, d)
      }
    }

  private final case class FoldRight[A, B](
      fa: RF[A],
      z: Need[B],
      f: (A, => B) => B
  )

  implicit def isFoldable: Foldable[RF] = new Foldable[RF] {

    def foldMap[A, B](fa: RF[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B =
      fa match {
        case Empty      => F.zero
        case Point(a)   => f(a.value)
        case Plus(a, b) => F.append(foldMap(a)(f), foldMap(b)(f))
        case Ap(a, b)   => foldMap(a)(c => foldMap(b)(d => f(d(c))))
      }

    def foldRight[A, B](fa: RF[A], z: => B)(f: (A, => B) => B): B = {
      fa match {
        case Empty      => z
        case Point(a)   => f(a.value, z)
        case Plus(a, b) => foldRight(a, foldRight(b, z)(f))(f)
        case Ap(a, b) =>
          foldRight(a, z)((c, y) => foldRight(b, y)((d, x) => f(d(c), x)))
      }
    }
  }
}
