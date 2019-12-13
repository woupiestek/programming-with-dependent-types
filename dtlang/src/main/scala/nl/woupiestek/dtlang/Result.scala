package nl.woupiestek.dtlang

import Result._
import scala.collection.mutable

case class Result[R, A](
    run: (R, Int) => Option[(A, Int)]
) extends AnyVal {

  def flatMap[B](
      f: A => Result[R, B]
  ): Result[R, B] =
    Result(
      (r, i) =>
        run(r, i) flatMap {
          case (e, j) => f(e).run(r, j)
        }
    )

  def map[B](f: A => B): Result[R, B] =
    Result(
      (r, i) =>
        run(r, i) map {
          case (v, j) => (f(v), j)
        }
    )

  def ap[B](f: Result[R, A => B]): Result[R, B] =
    Result(
      (r, i) =>
        run(r, i) flatMap {
          case (b, j) =>
            f.run(r, j) map {
              case (g, k) => (g(b), k)
            }
        }
    )

  def **[B](f: Result[R, A => B]): Result[R, B] = ap(f)

  def filter(f: A => Boolean): Result[R, A] =
    flatMap(a => if (f(a)) point(a) else empty[R, A])

  def plus(p: Result[R, A]): Result[R, A] =
    Result((r, i) => run(r, i) orElse p.run(r, i))

  def ++(p: Result[R, A]) = plus(p)

  def local[S](f: S => R): Result[S, A] =
    Result((s, i) => run(f(s), i))

  def move(i: Int): Result[R, A] = Result(
    (r, j) =>
      run(r, j).map {
        case (e, k) => (e, k + i)
      }
  )

  def memoized: Result[R, A] = {
    val visited: mutable.Set[(R, Int)] =
      new mutable.HashSet[(R, Int)]
    val memo: mutable.Map[(R, Int), (A, Int)] =
      new mutable.HashMap()

    def runMemoized(
        r: R,
        i: Int
    ): Option[(A, Int)] = {
      if (visited((r, i))) {
        memo.get((r, i))
      } else {
        visited.add((r, i))
        val o = run(r, i)
        o.foreach {
          case (a, j) => memo.put((r, i), (a, j))
        }
        o
      }
    }
    Result(runMemoized)
  }
}

object Result {
  def empty[R, A]: Result[R, A] =
    Result((_: R, _) => None)
  def point[R, A](a: A): Result[R, A] =
    Result[R, A]((_, i) => Some((a, i)))
  def context[R, E]: Result[R, R] =
    Result((r: R, i) => Some((r, i)))
  def read[R, A](f: R => A): Result[R, A] =
    Result((r: R, i) => Some((f(r), i)))
  def position[R, E]: Result[R, Int] =
    Result((_: R, i) => Some((i, i)))
}
