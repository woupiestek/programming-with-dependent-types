package nl.woupiestek.andrej

import nl.woupiestek.andrej.Free.Bind

import scala.annotation.tailrec
import scala.language.higherKinds

sealed trait Free[F[_], T] {
  def fold[U](x: Free.Folder[F, U], y: T => U): U

  final def flatMap[U](f: T => Free[F, U]): Free[F, U] =
    fold(new Free.Folder[F, Free[F, U]] {
      override def fold[W]
          : (F[W], (W) => Free[F, U]) => Bind[F, W, U] =
        Free.Bind[F, W, U]
    }, f)

  final def map[U](f: T => U): Free[F, U] =
    flatMap(x => Free(f(x)))
}

object Free {

  trait Folder[F[_], V] {
    def fold[U]: (F[U], U => V) => V
  }

  case class Return[F[_], T](value: T) extends Free[F, T] {
    override def fold[U](x: Folder[F, U], y: T => U): U =
      y(value)
  }

  case class Bind[F[_], E, T](
      request: F[E],
      callback: E => Free[F, T]
  ) extends Free[F, T] {
    override def fold[U](f: Folder[F, U], y: T => U): U =
      f.fold(request, (e: E) => callback(e).fold(f, y))
  }

  def apply[F[_], T](t: T): Free[F, T] = Return(t)

  def lift[F[_], T](ft: F[T]): Free[F, T] =
    Bind[F, T, T](ft, apply)
}

trait StraightInterpreter[F[_]] {
  def evaluate[T](ft: F[T]): T

  @tailrec final def execute[T](fft: Free[F, T]): T =
    fft match {
      case Free.Return(t) => t
      case Free.Bind(r, c) =>
        execute(c.asInstanceOf[Any => Free[F, T]](evaluate(r)))
    }
}

trait ReproducingInterpreter[F[_]] {

  def evaluate[T](ft: F[T]): (T, ReproducingInterpreter[F])

  @tailrec final def execute[T](fft: Free[F, T]): T =
    fft match {
      case Free.Return(t) => t
      case Free.Bind(r, c) =>
        evaluate(r) match {
          case (x, y) =>
            y.execute(c.asInstanceOf[Any => Free[F, T]](x))
        }
    }
}

trait CallbackInterpreter[F[_]] extends Free.Folder[F, Unit] {
  def fold[T](ft: F[T], cb: T => Unit): Unit

  //since this is not tail recursive, evaluate had better know what it is doing
  final def execute[T](fft: Free[F, T], cb: T => Unit): Unit =
    fft.fold(this, cb)
}

trait GoesInterpreter[F[_], G[_]] {

  def evaluate[T](t: F[T]): Free[G, T]

  def execute[T](fft: Free[F, T]): Free[G, T] = fft match {
    case Free.Return(t) => Free.Return(t)
    case Free.Bind(r, c) =>
      evaluate(r).flatMap(
        x => execute(c.asInstanceOf[Any => Free[F, T]](x))
      )
  }

}
