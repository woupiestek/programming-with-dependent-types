package nl.woupiestek.dtlang

import Result._

case class Result[R, E, A](
    run: (R, Int) => Option[(Either[E, A], Int)]
) extends AnyVal {

  def flatMap[B](
      f: A => Result[R, E, B]
  ): Result[R, E, B] =
    follow {
      case Right(a) => f(a)
      case Left(e)  => fail(e)
    }

  def recover[B](
      f: E => Result[R, E, A]
  ): Result[R, E, A] =
    follow {
      case Right(a) => point(a)
      case Left(e)  => f(e)
    }

  def follow[B](
      f: Either[E, A] => Result[R, E, B]
  ): Result[R, E, B] =
    Result(
      (r, i) =>
        run(r, i) flatMap {
          case (e, j) => f(e).run(r, j)
        }
    )

  def map[B](f: A => B): Result[R, E, B] =
    Result(
      (r, i) =>
        run(r, i) map {
          case (v, j) => (v.map(f), j)
        }
    )

  def ap[B](f: Result[R, E, A => B]): Result[R, E, B] =
    Result(
      (r, i) =>
        run(r, i) flatMap {
          case (b, j) =>
            f.run(r, j) map {
              case (g, k) => (b.flatMap(c => g.map(_(c))), k)
            }
        }
    )

  def **[B](f: Result[R, E, A => B]): Result[R, E, B] = ap(f)

  def filter(f: A => Boolean): Result[R, E, A] =
    flatMap(a => if (f(a)) point(a) else empty[R, E, A])

  def plus(p: Result[R, E, A]): Result[R, E, A] =
    Result((r, i) => run(r, i) orElse p.run(r, i))

  def ++(p: Result[R, E, A]) = plus(p)

  def local[S](f: S => R): Result[S, E, A] =
    Result((s, i) => run(f(s), i))

  def move(i: Int): Result[R, E, A] = Result(
    (r, j) =>
      run(r, j).map {
        case (e, k) => (e, k + i)
      }
  )

}

object Result {
  def empty[R, E, A]: Result[R, E, A] =
    Result((_: R, _) => None)
  def point[R, E, A](a: A): Result[R, E, A] =
    Result[R, E, A]((_, i) => Some((Right(a), i)))
  def fail[R, E, A](e: E): Result[R, E, A] =
    Result[R, E, A]((_, i) => Some((Left(e), i)))
  def context[R, E]: Result[R, E, R] =
    Result((r: R, i) => Some((Right(r), i)))
  def read[R, E, A](f: R => A): Result[R, E, A] =
    Result((r: R, i) => Some((Right(f(r)), i)))
  def position[R, E]: Result[R, E, Int] =
    Result((_: R, i) => Some((Right(i), i)))
}
