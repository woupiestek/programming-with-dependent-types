package nl.woupiestek.equalizer.parsing

//eventually support concurrency somehow
case class Parser4[I, E, A](
    val run: Parser4.Input[I] => Parser4.Result[I, E, A]
) extends AnyVal {

  def flatMap[B](f: A => Parser4[I, E, B]): Parser4[I, E, B] =
    Parser4(run(_).flatMap {
      case (a, i) => f(a).run(i)
    })

  def map[B](f: A => B): Parser4[I, E, B] =
    Parser4(run(_).map { case (a, i) => (f(a), i) })

  def filter(f: A => Boolean): Parser4[I, E, A] =
    Parser4(run(_).flatMap {
      case (a, i) => if (f(a)) Right((a, i)) else Left(None)
    })

  final def plus(
      p: Parser4[I, E, A]
  ): Parser4[I, E, A] =
    Parser4(
      i =>
        run(i) match {
          case Left(None) => p.run(i)
          case other      => other
        }
    )
  final def ++(p: Parser4[I, E, A]): Parser4[I, E, A] = plus(p)
}

object Parser4 {

  type Result[I, E, A] =
    Either[Option[E], (A, Parser4.Input[I])]

  def empty[I, E, A]: Parser4[I, E, A] =
    Parser4(_ => Left(None))
  def error[I, E, A](e: => E): Parser4[I, E, A] =
    Parser4(_ => Left(Some(e)))
  def point[I, E, A](a: => A): Parser4[I, E, A] =
    Parser4(i => Right((a, i)))
  def read[I, E]: Parser4[I, E, I] = Parser4(
    input => Right((input.head, input.tail))
  )

  //should become a comonad
  //lazy, memoized, etc.
  class Input[I](h: => I, t: => Input[I]) {
    lazy val head: I = h
    lazy val tail: Input[I] = t
    def extend[J](f: Input[I] => J): Input[J] =
      new Input(f(this), tail.extend(f))
  }

  def constant[I](i: => I): Input[I] = {
    lazy val j: Input[I] = new Input(i, j)
    j
  }

  def fromString(string: String): Input[Char] = {
    val eof = (-1).toChar
    string.toList.foldRight(constant(eof))(
      new Input(_, _)
    )
  }

}
