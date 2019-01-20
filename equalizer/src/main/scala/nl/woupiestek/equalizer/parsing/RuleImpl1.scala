package nl.woupiestek.equalizer.parsing

final case class RuleImpl1[I, O](
  emit: () => List[O],
  next: I => RuleImpl1[I, O])

object RuleImpl1 {

  implicit def instance[I]: Rule[RuleImpl1, I] = new Rule[RuleImpl1, I] {
    override def readIf(f: I => Boolean): RuleImpl1[I, I] =
      RuleImpl1(() => List.empty[I], point[I](_))

    override def ap[A, B](fa: => RuleImpl1[I, A])(f: => RuleImpl1[I, A => B]): RuleImpl1[I, B] =
      RuleImpl1(() => for {
        a <- fa.emit()
        g <- f.emit()
      } yield g(a), i => ap(fa.next(i))(f.next(i)))

    override def empty[A]: RuleImpl1[I, A] =
      RuleImpl1[I, A](() => Nil, _ => empty)

    override def plus[A](a: RuleImpl1[I, A], b: => RuleImpl1[I, A]): RuleImpl1[I, A] =
      RuleImpl1(() => a.emit() ++ b.emit(), i => plus(a.next(i), b.next(i)))

    override def point[A](a: => A): RuleImpl1[I, A] =
      RuleImpl1(() => List(a), _ => empty)
  }

}