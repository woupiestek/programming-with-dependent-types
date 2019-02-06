package nl.woupiestek.equalizer.parsing
import scalaz._

final case class RuleImpl1[I, O](
    emit: () => List[O],
    next: I => RuleImpl1[I, O]
)

object RuleImpl1 {
  implicit def isApplicativePlus[I]
      : ApplicativePlus[({ type F[O] = RuleImpl1[I, O] })#F] = {
    type F[O] = RuleImpl1[I, O]
    new ApplicativePlus[F] {
      override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
        RuleImpl1(
          () =>
            for {
              a <- fa.emit()
              g <- f.emit()
            } yield g(a),
          i => ap(fa.next(i))(f.next(i))
        )

      override def empty[A]: F[A] =
        RuleImpl1(() => Nil, _ => empty)

      override def plus[A](a: F[A], b: => F[A]): F[A] =
        RuleImpl1(
          () => a.emit() ++ b.emit(),
          i => plus(a.next(i), b.next(i))
        )

      override def point[A](a: => A): F[A] =
        RuleImpl1(() => List(a), _ => empty)
    }
  }
}
