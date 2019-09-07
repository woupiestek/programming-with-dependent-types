package nl.woupiestek.equalizer.parsing
import Parser4._
//eventually support concurrency somehow
case class Parser4[I, A](
    val run: Input[I] => Parser4.Result[I, A]
) extends AnyVal {

  def flatMap[B](f: A => Parser4[I, B]): Parser4[I, B] =
    Parser4 { (ia: Input[I]) =>
      val pbs: Input[Parser4.Result[I, A]] = ia.extend(run)
      pbs.head.value match {
        case None    => Result(pbs.head.input, None)
        case Some(a) => f(a).run(pbs.map(_.input))
      }
    }

  def map[B](f: A => B): Parser4[I, B] =
    Parser4 { (ia: Input[I]) =>
      val result = run(ia)
      result.copy(
        value = result.value.map(f)
      )
    }

  def filter(f: A => Boolean): Parser4[I, A] =
    Parser4 { (ia: Input[I]) =>
      val result = run(ia)
      result.copy(value = result.value.filter(f))
    }

  final def plus(
      p: Parser4[I, A]
  ): Parser4[I, A] =
    Parser4 { (ia: Input[I]) =>
      val pbs: Input[Parser4.Result[I, A]] = ia.extend(run)
      pbs.head.value match {
        case None    => p.run(ia)
        case Some(_) => pbs.head
      }
    }

  final def ++(p: Parser4[I, A]): Parser4[I, A] = plus(p)
}

object Parser4 {

  case class Result[I, A](input: I, value: Option[A])

  def empty[I, A]: Parser4[I, A] =
    Parser4((ia: Input[I]) => Result(ia.head, None))
  def point[I, A](a: => A): Parser4[I, A] =
    Parser4((ia: Input[I]) => Result(ia.head, Some(a)))
  def read[I, E]: Parser4[I, I] = Parser4(
    (ia: Input[I]) => Result(ia.tail.head, Some(ia.head))
  )

}
