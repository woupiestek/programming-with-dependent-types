package nl.woupiestek.equalizer.parsing
import scala.collection.mutable

case class Parser5[I, A](
    run: (Int => I, Int) => List[(Int, A)]
) extends AnyVal {
  def flatMap[B](f: A => Parser5[I, B]): Parser5[I, B] =
    Parser5(
      (b, a) =>
        run(b, a).flatMap {
          case (c, d) => f(d).run(b, c)
        }
    )

  def map[B](f: A => B): Parser5[I, B] =
    compose(
      _.map {
        case (c, d) => (c, f(d))
      }
    )

  def filter(f: A => Boolean): Parser5[I, A] =
    compose(
      _.filter {
        case (_, d) => f(d)
      }
    )

  def plus(p: Parser5[I, A]): Parser5[I, A] =
    Parser5(
      (b, a) => run(b, a) ++ p.run(b, a)
    )

  final def ++(p: Parser5[I, A]): Parser5[I, A] = plus(p)

  private def compose[B](
      f: List[(Int, A)] => List[(Int, B)]
  ): Parser5[I, B] =
    Parser5(
      (b, a) => f(run(b, a))
    )

  def memoized: Parser5[I, A] =
    Parser5((b, a) => Parser5.Memoize(run(b, _))(a))
}

object Parser5 {
  def empty[I, A]: Parser5[I, A] = Parser5[I, A]((_, _) => Nil)
  def point[I, A](a: => A): Parser5[I, A] =
    Parser5[I, A]((_, i) => List((i, a)))
  def read[I]: Parser5[I, I] =
    Parser5[I, I]((b, a) => List((a + 1, b(a))))

  def unit[A](
      f: (Int => A, Int) => List[Int]
  ): Parser5[A, Unit] =
    Parser5[A, Unit]((b, a) => f(b, a).map((_, ())))

  case class Memoize[B](f: Int => List[(Int, B)])
      extends (Int => List[(Int, B)]) {
    private val memory: mutable.Map[Int, List[(Int, B)]] =
      new mutable.HashMap[Int, List[(Int, B)]]

    def apply(a: Int): List[(Int, B)] =
      memory.getOrElseUpdate(a, f(a))
  }
}
