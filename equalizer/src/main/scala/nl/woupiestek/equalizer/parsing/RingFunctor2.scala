package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import RingFunctor2._
import scala.annotation.tailrec

final class RingFunctor2[A] private (value: => Maybe[NonZero[_, A]]) {
  private def get = value
}

object RingFunctor2 {
  type RF[A] = RingFunctor2[A]

  private sealed abstract class Arrow[A, B]
  private final case class Wrap[A, B](nz: NonZero[_, A => B])
      extends Arrow[A, B]
  private final case class Identity[A]() extends Arrow[A, A]
  private final case class AndThen[A, B, C](
      head: Arrow[A, B],
      tail: Arrow[B, C]
  ) extends Arrow[A, C]

  //we run into the same awkward agregation problems twice.
  private final case class NonZero[A, B](
      head: Need[A],
      tail: Arrow[A, B],
      rest: RF[B]
  )

  implicit def isApplicativePlus: ApplicativePlus[RF] =
    new ApplicativePlus[RF] {
      // Members declared in scalaz.Applicative
      def point[A](a: => A): RF[A] =
        new RF(Maybe.Just(NonZero(Need(a), Identity[A](), new RF(Maybe.empty))))

      // Members declared in scalaz.Apply
      def ap[A, B](fa: => RF[A])(f: => RF[A => B]): RF[B] = {
        def _ap(ga: NonZero[_, A], g: NonZero[_, A => B]): NonZero[_, B] =
          NonZero(
            ga.head,
            AndThen(ga.tail.asInstanceOf[Arrow[Any, A]], Wrap(g)),
            new RF(ga.rest.get.map(a => _ap(a, g)))
          )

        new RF((fa.get |@| f.get)(_ap))
      }

      // Members declared in scalaz.Plus
      def plus[A](a: RF[A], b: => RF[A]): RF[A] = {
        def _plus(a: NonZero[_, A], b: NonZero[_, A]): NonZero[_, A] =
          NonZero(
            a.head,
            a.tail.asInstanceOf[Arrow[Any, A]],
            new RF(a.rest.get.map(_plus(_, b)))
          )
        new RF((a.get |@| b.get)(_plus))
      }

      // Members declared in scalaz.PlusEmpty
      def empty[A]: RF[A] = new RF(Maybe.empty)

    }

  implicit def isFoldable: Foldable[RF] = new Foldable[RF] {
    def foldMap[A, B](fa: RF[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B = {
      @tailrec def fm[A0](acc: B, rf: RF[A]): B = rf.get match {
        case Maybe.Empty() => acc
        case Maybe.Just(NonZero(head, tail, rest)) =>
          if (tail == Identity()) //subfoldmap game...
            fm(F.append(acc, f(head.value.asInstanceOf[A])), rest)
          else ???
      }

      fm(F.zero, fa)
    }

    def foldRight[A, B](fa: RF[A], z: => B)(f: (A, => B) => B): B = ???

  }

}
