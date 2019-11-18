package nl.woupiestek.equalizer.parsing
import scala.collection.mutable

case class Parser3[I, A](
    run: (Int => I) => Int => Option[(Int, A)]
) extends AnyVal {

  def flatMap[B](f: A => Parser3[I, B]): Parser3[I, B] =
    Parser3(
      b =>
        a =>
          run(b)(a).flatMap {
            case (c, d) => f(d).run(b)(c)
          }
    )

  def map[B](f: A => B): Parser3[I, B] =
    Parser3(
      b =>
        a =>
          run(b)(a).map {
            case (c, d) => (c, f(d))
          }
    )

  def filter(f: A => Boolean): Parser3[I, A] =
    Parser3(
      b =>
        a =>
          run(b)(a).filter {
            case (_, d) => f(d)
          }
    )

  def plus(p: Parser3[I, A]): Parser3[I, A] =
    Parser3(
      b => a => run(b)(a).orElse(p.run(b)(a))
    )

  final def ++(p: Parser3[I, A]): Parser3[I, A] = plus(p)

  def memoized: Parser3[I, A] =
    Parser3(b => Parser3.Memoize(run(b)))

  def derive(i: I): Parser3[I, A] =
    Parser3(b => run(j => if (j > 0) b(j - 1) else i))

}

object Parser3 {

  def empty[I, A]: Parser3[I, A] = Parser3[I, A](_ => _ => None)
  def point[I, A](a: => A): Parser3[I, A] =
    Parser3[I, A](_ => i => Some((i, a)))
  def read[I]: Parser3[I, I] =
    Parser3[I, I](b => a => Some((a + 1, b(a))))

  def unit[A](
      f: (Int => A) => Int => Option[Int]
  ): Parser3[A, Unit] =
    Parser3[A, Unit](b => a => f(b)(a).map((_, ())))

  case class Memoize[B](f: Int => Option[(Int, B)])
      extends (Int => Option[(Int, B)]) {
    private val memory: mutable.Map[Int, Option[(Int, B)]] =
      new mutable.HashMap[Int, Option[(Int, B)]]

    def apply(a: Int): Option[(Int, B)] = {
      memory.getOrElse(a, {
        val b = f(a)
        memory.put(a, b)
        b
      })
    }
  }
}
