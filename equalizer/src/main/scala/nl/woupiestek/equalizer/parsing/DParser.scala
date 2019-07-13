package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class DParser[-I, +O]

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
    read.flatMap((i: I) => if (f(i)) write(i) else empty)
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
        if (a == Empty) b else if (b == Empty) a else Plus(a, b)
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

  def derive[I, O](dp: DParser[I, O], i: I): DParser[I, O] =
    fold((_: I => DParser[I, O])(i), (_: O) => Empty)(dp)

  def matches[I, O](dp: DParser[I, O]): Stream[O] =
    fold((_: I => Stream[O]) => Stream.empty, Stream(_: O))(dp)

}
