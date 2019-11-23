package nl.woupiestek.equalizer.parsing

case class Parser4[I, A](
    val run: Input[I] => Parser4.Result[I, A]
) extends AnyVal {

  def flatMap[B](f: A => Parser4[I, B]): Parser4[I, B] =
    Parser4(
      (ia: Input[I]) =>
        run(ia).flatMap {
          case (i, a) => f(a).run(i)
        }
    )

  def map[B](f: A => B): Parser4[I, B] =
    Parser4(
      (ia: Input[I]) =>
        run(ia).map {
          case (i, a) => (i, f(a))
        }
    )

  def filter(f: A => Boolean): Parser4[I, A] =
    Parser4(
      (ia: Input[I]) =>
        run(ia).filter {
          case (_, a) => f(a)
        }
    )

  final def plus(
      p: Parser4[I, A]
  ): Parser4[I, A] =
    Parser4((ia: Input[I]) => run(ia) orElse p.run(ia))

  final def ++(p: Parser4[I, A]): Parser4[I, A] = plus(p)
}

object Parser4 {

  type Result[I, A] = Option[(Input[I], A)]

  def empty[I, A]: Parser4[I, A] =
    Parser4((_: Input[I]) => None)
  def point[I, A](a: => A): Parser4[I, A] =
    Parser4((ia: Input[I]) => Some((ia, a)))
  def read[I, E]: Parser4[I, I] = Parser4(
    (ia: Input[I]) => Some((ia.tail, ia.head))
  )

}
