package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

sealed abstract class DParser[-I, +O]

object DParser {

  private final case object Empty extends DParser[Any, Nothing]
  private final case object Read extends DParser[Any, Nothing]
  private final case class Sum[I, O](left: DParser[I, O], right: DParser[I, O])
      extends DParser[I, O]
  private final case class Write[O](o: O) extends DParser[Any, O]
  private final case class FlatMap[I, O, P](
      dpi: DParser[I, O],
      dpj: O => DParser[I, P]
  ) extends DParser[I, P]

  def read[I, O]: DParser[I, O] = Read
  def write[I, O](o: O): DParser[I, O] = Write(o)
  private val Pause = Write(())
  def suspend[I, O](dpi: => DParser[I, O]): DParser[I, O] =
    FlatMap(Pause, (_: Unit) => dpi)
  def empty[I, O]: DParser[I, O] = Empty

  private class Instances[I] {
    type F[+O] = DParser[I, O]
    val monadPlus: MonadPlus[F] = new MonadPlus[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
        fa match {
          case Empty               => Empty
          case g: FlatMap[I, x, A] => FlatMap(g.dpi, g.dpj(_: x).flatMap(f))
          case Write(o)            => f(o)
          case _                   => FlatMap(fa, f)
        }
      def empty[A]: F[A] = Empty
      def plus[A](a: F[A], b: => F[A]): F[A] =
        if (a == Empty) b else if (b == Empty) a else Sum(a, b)
      def point[A](a: => A): F[A] = Write(a)
    }
  }

  implicit def monadPlus[I]: MonadPlus[({ type F[+O] = DParser[I, O] })#F] =
    new Instances[I].monadPlus
  implicit def monoid[I, O]: Monoid[DParser[I, O]] = monadPlus.monoid[O]

  type Out[-I, +O] = Stream[Either[I => DParser[I, O], O]]

  def stream[I, O, Z](read: (I => DParser[I, O]) => Z, write: O => Z)(
      dp: DParser[I, O]
  ): Stream[Z] = {
    def helper1[X](
        dpx: DParser[I, X],
        f: X => DParser[I, O],
        out: DParser[I, O]
    ): Stream[Z] = dpx match {
      case Empty               => stream(read, write)(out)
      case g: FlatMap[I, w, X] => helper1(g.dpi, g.dpj(_: w).flatMap(f), out)
      case Read =>
        read((i: I) => f(i.asInstanceOf[X])) #:: stream(read, write)(out)
      case Sum(left, right) => helper1(left, f, Sum(right.flatMap(f), out))
      case Write(x)         => helper0(f(x), out)
    }

    def helper0(
        dpx: DParser[I, O],
        out: DParser[I, O]
    ): Stream[Z] = dpx match {
      case Empty               => stream(read, write)(out)
      case f: FlatMap[I, x, O] => helper1(f.dpi, f.dpj, out)
      case Read =>
        read((i: I) => Write(i.asInstanceOf[O])) #:: stream(read, write)(out)
      case Sum(left, right) => helper0(left, Sum(right, out))
      case Write(o)         => write(o) #:: stream(read, write)(out)
    }

    helper0(dp, Empty)
  }

  //P => 1 + (O + (I => P)) * P
  def derive[I, O](dp: DParser[I, O], i: I): DParser[I, O] = 
    stream((_: I => DParser[I, O])(i), (_: O) => Empty)(dp).fold(Empty)(_ <+> _)
  

  def matches[I, O](dp: DParser[I, O]): List[O] = 
    stream((_: I => DParser[I, O]) => Nil, List(_: O))(dp).flatten.toList
  

}
