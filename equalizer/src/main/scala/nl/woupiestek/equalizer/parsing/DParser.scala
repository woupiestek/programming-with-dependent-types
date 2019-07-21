package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class DParser[-I, +O] {
  def isEmpty: Boolean = this == DParser.Empty
}

object DParser {

  private final case object Empty extends DParser[Any, Nothing]
  private final case object Read extends DParser[Any, Nothing]
  private final case class Plus[I, O](left: DParser[I, O], right: DParser[I, O])
      extends DParser[I, O]
  private final case class Write[O](o: O) extends DParser[Any, O]
  private final case class FlatMap[I, O, P](
      dpo: DParser[I, O],
      dpp: O => DParser[I, P]
  ) extends DParser[I, P]

  def read[I]: DParser[I, I] = Read
  def readIf[I](f: I => Boolean): DParser[I, I] =
    monadPlus.bind(read[I])((i: I) => if (f(i)) write(i) else empty)
  def write[I, O](o: O): DParser[I, O] = Write(o)
  private val Pause = Write(())
  def suspend[I, O](dpo: => DParser[I, O]): DParser[I, O] =
    FlatMap(Pause, (_: Unit) => dpo)
  def empty[I, O]: DParser[I, O] = Empty

  private class Instances[I] {
    type F[+O] = DParser[I, O]
    val monadPlus: MonadPlus[F] = new MonadPlus[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
        fa match {
          case Empty               => Empty
          case g: FlatMap[I, x, A] => FlatMap(g.dpo, g.dpp(_: x).flatMap(f))
          case Write(o)            => f(o)
          case _                   => FlatMap(fa, f)
        }
      def empty[A]: F[A] = Empty
      def plus[A](a: F[A], b: => F[A]): F[A] =
        if (a.isEmpty) b else if (b.isEmpty) a else Plus(a, b)
      def point[A](a: => A): F[A] = Write(a)
    }
  }

  implicit def monadPlus[I]: MonadPlus[({ type F[+O] = DParser[I, O] })#F] =
    new Instances[I].monadPlus
  implicit def monoid[I, O]: Monoid[DParser[I, O]] = monadPlus.monoid[O]

  def fold[I, O, Z: Monoid](read: (I => Z) => Z, write: O => Z)(
      dp: DParser[I, O]
  ): Z = {
    def helper1[X](dpx: DParser[I, X], f: X => DParser[I, O]): Z = dpx match {
      case Empty               => Monoid[Z].zero
      case g: FlatMap[I, w, X] => helper1(g.dpo, g.dpp(_: w).flatMap(f))
      case Read                => read((i: I) => helper0(f(i.asInstanceOf[X])))
      case Plus(left, right)   => helper1(left, f) |+| helper1(right, f)
      case Write(x)            => helper0(f(x))
    }

    def helper0(dpx: DParser[I, O]): Z = dpx match {
      case Empty             => Monoid[Z].zero
      case FlatMap(dpo, dpp) => helper1(dpo, dpp)
      case Read              => read((i: I) => write(i.asInstanceOf[O]))
      case Plus(left, right) => helper0(left) |+| helper0(right)
      case Write(o)          => write(o)
    }

    helper0(dp)
  }

  def fold2[I, O, Z: Monoid](
      recursionLimit: Int
  )(read: Option[I], write: O => Z)( //todo: create two methods
      dp: DParser[I, O]
  ): Z = {

    def helper1[X](
        dpx: DParser[I, X],
        f: X => DParser[I, O],
        limit: Int,
        todo: DParser[I, O]
    ): (Z, DParser[I, O]) = {
      if (limit > 0) {
        dpx match {
          case Empty =>
            todo match {
              case Empty => (Monoid[Z].zero, Empty)
              case _     => helper0(todo, limit - 1, Empty)
            }
          case g: FlatMap[I, w, X] =>
            helper1(g.dpo, g.dpp(_: w).flatMap(f), limit - 1, todo)
          case Read =>
            read match {
              case None =>
                todo match {
                  case Empty => (Monoid[Z].zero, Empty)
                  case _     => helper0(todo, limit - 1, Empty)
                }
              case Some(i) => helper0(f(i.asInstanceOf[X]), limit - 1, todo)
            }
          case Plus(left, right) =>
            helper1(left, f, limit - 1, right.flatMap(f) |+| todo)
          case Write(x) => helper0(f(x), limit - 1, todo)
        }
      } else {
        (Monoid[Z].zero, dpx.flatMap(f) |+| todo)
      }
    }

    def helper0(
        dpx: DParser[I, O],
        limit: Int,
        todo: DParser[I, O]
    ): (Z, DParser[I, O]) =
      if (limit > 0) {
        dpx match {
          case Empty =>
            todo match {
              case Empty => (Monoid[Z].zero, Empty)
              case _     => helper0(todo, limit - 1, Empty)
            }
          case FlatMap(dpo, dpp) => helper1(dpo, dpp, limit - 1, todo)
          case Read =>
            read match {
              case None =>
                todo match {
                  case Empty => (Monoid[Z].zero, Empty)
                  case _     => helper0(todo, limit - 1, Empty)
                }
              case Some(i) => (write(i.asInstanceOf[O]), todo)
            }
          case Plus(left, right) =>
            helper0(left, limit - 1, right |+| todo)
          case Write(o) => (write(o), todo)
        }
      } else {
        (Monoid[Z].zero, dpx |+| todo)
      }

    var done = Monoid[Z].zero
    var todo = dp
    while (!todo.isEmpty) {
      val (d, t) = helper0(todo, recursionLimit, Empty)
      todo = t
      done = done |+| d
    }
    done
  }

  def derive[I, O](dp: DParser[I, O], i: I): DParser[I, O] =
    fold((_: I => DParser[I, O])(i), (_: O) => Empty)(dp)

  def matches[I, O](dp: DParser[I, O]): Stream[O] =
    fold((_: I => Stream[O]) => Stream.empty, Stream(_: O))(dp)

  def derive2[I, O](
      recursionLimit: Int
  )(i: I)(
      dp: DParser[I, O]
  ): DParser[I, O] = {

    def helper1[X](
        dpx: DParser[I, X],
        f: X => DParser[I, O],
        limit: Int,
        todo: DParser[I, O]
    ): (DParser[I, O], DParser[I, O]) = {
      if (limit > 0) {
        dpx match {
          case Empty | Write(_) =>
            todo match {
              case Empty | Write(_) => (Empty, Empty)
              case _                => helper0(todo, limit - 1, Empty)
            }
          case g: FlatMap[I, w, X] =>
            helper1(g.dpo, g.dpp(_: w).flatMap(f), limit - 1, todo)
          case Read =>
            val d0 = f(i.asInstanceOf[X])
            val (d1, t) = helper0(todo, limit - 1, Empty)
            (d0 |+| d1, t)
          case Plus(left, right) =>
            helper1(left, f, limit - 1, right.flatMap(f) |+| todo)
        }
      } else {
        (Empty, dpx.flatMap(f) |+| todo)
      }
    }

    def helper0(
        dpx: DParser[I, O],
        limit: Int,
        todo: DParser[I, O]
    ): (DParser[I, O], DParser[I, O]) =
      if (limit > 0) {
        dpx match {
          case Empty | Write(_) =>
            todo match {
              case Empty | Write(_) => (Empty, Empty)
              case _                => helper0(todo, limit - 1, Empty)
            }
          case FlatMap(dpo, dpp) => helper1(dpo, dpp, limit - 1, todo)
          case Read =>
            val (d, t) = helper0(todo, limit - 1, Empty)
            (write[I, O](i.asInstanceOf[O]) |+| d, t)
          case Plus(left, right) =>
            helper0(left, limit - 1, right |+| todo)
        }
      } else {
        (Empty, dpx |+| todo)
      }

    var done = empty[I, O]
    var todo = dp
    while (!todo.isEmpty) {
      val (d, t) = helper0(todo, recursionLimit, Empty)
      todo = t
      done = done |+| d
    }
    done
  }
}
