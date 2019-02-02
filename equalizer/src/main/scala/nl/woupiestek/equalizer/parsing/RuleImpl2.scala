package nl.woupiestek.equalizer.parsing

final case class RuleImpl2[I, O](
    emit: () => List[O],
    next: () => RuleImpl2[I, I => Option[O]]
)

object RuleImpl2 {

  implicit def instance[I]: Rule[({ type F[O] = RuleImpl2[I, O] })#F, I] = {
    type F[O] = RuleImpl2[I, O]
    new Rule[F, I] {
      override def readIf(f: I => Boolean): F[I] =
        RuleImpl2(() => Nil, () => point(Some(_).filter(f)))

      override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = {
        val h: F[B] = RuleImpl2(
          () => Nil,
          () =>
            ap(fa.next())(
              map(f)((a: A => B) => (b: I => Option[A]) => b(_).map(a))
            )
        )
        fa.emit().foldLeft(h)((fb, a) => plus(fb, map(f)(g => g(a))))
      }

      override def empty[A]: F[A] = RuleImpl2(() => Nil, () => empty)

      override def plus[A](a: F[A], b: => F[A]): F[A] =
        RuleImpl2(() => a.emit() ++ b.emit(), () => plus(a.next(), b.next()))

      override def point[A](a: => A): F[A] =
        RuleImpl2(() => List(a), () => empty)
    }
  }
}
