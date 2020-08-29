package nl.woupiestek.equalizer.fold

case class Monoid[M](empty: M, merge: (=> M, => M) => M)

trait MonoidK[F[+_]] {
  val empty: F[Nothing]
  def merge[A](fa: => F[A], fb: => F[A]): F[A]
  def monoid[A]: Monoid[F[A]] =
    Monoid[F[A]](empty, merge[A])
}

trait Fold[A, +B] {
  def foldMap[C](a: A, f: B => C)(implicit monoid: Monoid[C]): C
}

object Fold {
  implicit class FoldOps[A](private val value: A)
      extends AnyVal {
    final def foldMap[B, C](
        f: B => C
    )(implicit fold: Fold[A, B], monoid: Monoid[C]): C =
      fold.foldMap(value, f)(monoid)
    final def flatMap[B, C](
        f: B => C
    )(implicit fold: Fold[A, B], monoid: Monoid[C]): C =
      foldMap(f)(fold, monoid)
    final def size(implicit fold: Fold[A, Any]): Int =
      foldMap((_: Any) => 1)(fold, Monoid(0, (_ + _)))
    final def forall[B](
        f: B => Boolean
    )(implicit fold: Fold[A, B]): Boolean =
      foldMap(f)(fold, Monoid(true, (_ && _)))
    final def exists[B](
        f: B => Boolean
    )(implicit fold: Fold[A, B]): Boolean =
      foldMap(f)(fold, Monoid(false, (_ || _)))
    final def foldLeft[B, C](
        f: (B, C) => C
    )(implicit fold: Fold[A, B]): C => C =
      foldMap((b: B) => f(b, _))(
        fold,
        Monoid[C => C](c => c, (f, g) => f.andThen(g))
      )
    final def foldRight[B, C](
        f: (C, B) => C
    )(implicit fold: Fold[A, B]): C => C =
      foldMap((b: B) => f(_, b))(
        fold,
        Monoid[C => C](c => c, (f, g) => f.compose(g))
      )
    final def filter[B](f: B => Boolean): Filtered[A, B] =
      Filtered(value, f)
    final def reverse: Reversed[A] = new Reversed(value)
    final def map[B, C](f: B => C): Mapped[A, B, C] =
      Mapped(value, f)
    final def list[B](implicit fold: Fold[A, B]): List[B] =
      foldMap((b: B) => List(b))(fold, Monoid(Nil, (_ ++ _)))
  }

  case class Filtered[A, B](
      wrapped: A,
      condition: B => Boolean
  )

  implicit def filteredFold[A, B](
      implicit fold: Fold[A, B]
  ): Fold[Filtered[A, B], B] =
    new Fold[Filtered[A, B], B] {
      override def foldMap[C](a: Filtered[A, B], f: B => C)(
          implicit monoid: Monoid[C]
      ): C =
        fold.foldMap(
          a.wrapped,
          b => if (a.condition(b)) f(b) else monoid.empty
        )
    }

  class Reversed[A](val reverse: A) extends AnyVal

  implicit def reversedFold[A, B](
      implicit fold: Fold[A, B]
  ): Fold[Reversed[A], B] =
    new Fold[Reversed[A], B] {
      override def foldMap[C](a: Reversed[A], f: B => C)(
          implicit monoid: Monoid[C]
      ): C =
        fold.foldMap(
          a.reverse,
          f
        )(Monoid(monoid.empty, (x, y) => monoid.merge(y, x)))
    }

  case class Mapped[A, B, C](
      wrapped: A,
      mapping: B => C
  )

  implicit def mappedFold[A, B, C](
      implicit fold: Fold[A, B]
  ): Fold[Mapped[A, B, C], C] =
    new Fold[Mapped[A, B, C], C] {
      override def foldMap[D](a: Mapped[A, B, C], f: C => D)(
          implicit monoid: Monoid[D]
      ): D =
        fold.foldMap(
          a.wrapped,
          b => f(a.mapping(b))
        )
    }
}

trait FoldK[F[+_]] {
  def foldMap[B, C](fa: F[B], f: B => C)(
      implicit monoid: Monoid[C]
  ): C

  def fold[A] =
    new Fold[F[A], A] {
      final override def foldMap[C](fa: F[A], f: A => C)(
          implicit monoid: Monoid[C]
      ): C = foldMap(fa, f)
    }
}

trait Singleton[F[+_]] {
  def singleton[A](a: A): F[A]
}

/* we need a number of assumptions to derive traverse from 
 * foldmap...
 */
trait Applicative[A[+_]] {
  def map0[B](f: () => B): A[B] = map1(unit, (_: Unit) => f())
  def map1[B, C](ab: A[B], f: B => C): A[C]

  val unit: A[Unit]

  def par[B, C](ab: A[B], ac: A[C]): A[(B, C)]
  def map2[B, C, D](
      ab: A[B],
      ac: A[C],
      f: (=> B, => C) => D
  ): A[D] =
    map1[(B, C), D](par(ab, ac), { case (b, c) => f(b, c) })
}

object Applicative {
  implicit def monoidFA[F[+_], A](
      implicit monoidA: Monoid[A],
      applicativeF: Applicative[F]
  ): Monoid[F[A]] =
    Monoid(
      applicativeF.map0(() => monoidA.empty),
      (x, y) =>
        applicativeF.map2(x, y, (u, v) => monoidA.merge(u, v))
    )

  implicit class FoldKOps[F[+_], A](private val value: F[A]) {
    final def traverse[G[+_], B](f: A => G[B])(
        implicit foldK: FoldK[F],
        monoidK: MonoidK[F],
        singleton: Singleton[F],
        applicative: Applicative[G]
    ): G[F[B]] =
      foldK
        .fold[A]
        .foldMap[G[F[B]]](
          value,
          a => applicative.map1(f(a), singleton.singleton[B])
        )(
          monoidFA[G, F[B]](monoidK.monoid[B], applicative)
        )
  }
}
