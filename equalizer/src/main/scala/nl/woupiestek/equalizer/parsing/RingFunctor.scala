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
}
