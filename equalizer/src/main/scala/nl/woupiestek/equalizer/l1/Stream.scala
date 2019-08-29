package nl.woupiestek.equalizer.l1

import scalaz._

trait FoldM[IO[_], S, +A] {

  def foldLeftM[B](s: S, b: B)(
      f: (B, A) => IO[B]
  )(implicit IO: Monad[IO]): IO[B]

  def foldRightM[B](s: S, b: => B)(
      f: (A, => B) => IO[B]
  )(implicit IO: Monad[IO]): IO[B]

  def foldMapM[B](s: S)(
      f: A => IO[B]
  )(implicit B: Monoid[B], IO: Monad[IO]): IO[B] = {
    def g(a: A, b: => B): IO[B] =
      IO.map(f(a))(B.append(_, b))
    foldRightM(s, B.zero)(g)
  }
}
