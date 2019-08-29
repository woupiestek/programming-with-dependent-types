package nl.woupiestek.dtlang

import scala.concurrent.Future

trait Reactive[T] {

  def observe[U](first: U, next: (T, U) => Future[U]): Future[U]

  def map[T2](f: T => T2): Reactive[T2] =
    new Reactive.Map(this, f)

  def flatMap[T2](f: T => Reactive[T2]): Reactive[T2] =
    new Reactive.FlatMap(this, f)

  def ap[T2](f: Reactive[T => T2]): Reactive[T2] =
    new Reactive.Ap(this, f)

  def filter(p: T => Boolean): Reactive[T] =
    new Reactive.WithFilter(this, p)

  def until(p: T => Boolean): Reactive[T] =
    new Reactive.Until(this, p)
}

object Reactive {

  class Map[T, T2](base: Reactive[T], f: T => T2)
      extends Reactive[T2] {
    override def observe[U](
        first: U,
        next: (T2, U) => Future[U]
    ): Future[U] = base.observe(first, (t, u) => next(f(t), u))
  }

  class FlatMap[T, T2](base: Reactive[T], f: T => Reactive[T2])
      extends Reactive[T2] {
    override def observe[U](
        first: U,
        next: (T2, U) => Future[U]
    ): Future[U] =
      base.observe(first, (t, u) => f(t).observe(u, next))
  }

  class Ap[T, T2](base: Reactive[T], f: Reactive[T => T2])
      extends Reactive[T2] {
    override def observe[U](
        first: U,
        next: (T2, U) => Future[U]
    ): Future[U] =
      base.observe(
        first,
        (t, u) => f.observe(u, (g, v) => next(g(t), v))
      )
  }

  class WithFilter[T](base: Reactive[T], pred: T => Boolean)
      extends Reactive[T] {
    override def observe[U](
        first: U,
        next: (T, U) => Future[U]
    ): Future[U] = {
      base.observe(
        first,
        (t, u) =>
          if (pred(t)) next(t, u) else Future.successful(u)
      )
    }
  }

  class Until[T](base: Reactive[T], pred: T => Boolean)
      extends Reactive[T] {
    override def observe[U](
        first: U,
        next: (T, U) => Future[U]
    ): Future[U] = {
      base.observe(
        first,
        (t, u) =>
          if (pred(t)) Future.successful(first) else next(t, u)
      )
    }
  }

}
