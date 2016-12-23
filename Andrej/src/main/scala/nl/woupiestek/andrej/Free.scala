package nl.woupiestek.andrej

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.Monad

sealed trait Free[F[_], T] {
  def fold[U](x: Free.Folder[F, U], y: T => U): U

  final def flatMap[U](f: T => Free[F, U]): Free[F, U] = fold(new Free.Folder[F, Free[F, U]] {
    override def fold[W] = Free.Bind[F, W, U]
  }, f)

  final def map[U](f: T => U): Free[F, U] = flatMap(x => Free(f(x)))
}

object Free {

  trait Folder[F[_], V] {
    def fold[U]: (F[U], U => V) => V
  }

  case class Return[F[_], T](value: T) extends Free[F, T] {
    override def fold[U](x: Folder[F, U], y: T => U) = y(value)
  }

  case class Bind[F[_], E, T](request: F[E], callback: E => Free[F, T]) extends Free[F, T] {
    override def fold[U](x: Folder[F, U], y: T => U): U = x.fold(request, x => callback(x).fold(x, y))
  }

  def apply[F[_], T](t: T): Free[F, T] = Return(t)

  def lift[F[_], T](ft: F[T]): Free[F, T] = Bind[F, T, T](ft, apply)
}

trait StraightInterpreter[F[_]] {
  def evaluate[T](ft: F[T]): T

  @tailrec final def execute[T](fft: Free[F, T]): T = fft match {
    case Free.Return(t) => t
    case Free.Bind(r, c) => execute(c(evaluate(r)))
  }
}

trait ReproducingInterpreter[F[_]] {

  def evaluate[T](ft: F[T]): (T, ReproducingInterpreter[F])

  @tailrec final def execute[T](fft: Free[F, T]): T = fft match {
    case Free.Return(t) => t
    case Free.Bind(r, c) =>
      evaluate(r) match {
        case (x, y) => y.execute(c(x))
      }
  }
}

trait CallbackInterpreter[F[_]] extends Free.Folder[F, Unit] {
  def fold[T](ft: F[T], cb: T => Unit): Unit

  //since this is not tail recursive, evaluate had better know what it is doing
  final def execute[T](fft: Free[F, T], cb: T => Unit): Unit = fft.fold(this, cb)
}

trait MonadicIntepreter[F[_], G[_] : Monad] {

  def evaluate[T](t: F[T]): G[T]

  def execute[T](fft: Free[F, T]): G[T] = fft.fold(new Free.Folder[F, G[T]] {
    override def fold[U]: (F[U], (U) => G[T]) => G[T] = (r, c) =>
      implicitly[Monad[G]].bind(evaluate(r))(x => execute(c(x)))
  },
    implicitly[Monad[G]].point)
}

trait GoesInterpreter[F[_], G[_]] {

  def evaluate[T](t: F[T]): Free[G, T]

  def execute[T](fft: Free[F, T]): Free[G, T] = fft match {
    case Free.Return(t) => Free.Return(t)
    case Free.Bind(r, c) => evaluate(r).flatMap(x => execute(c(x)))
  }

}

