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

  def first[I, O](dp: DParser[I, O]): DParser[I, O] = {

    def helper1[X](
        dpx: DParser[I, X],
        f: X => DParser[I, O],
        out: DParser[I, O]
    ): DParser[I, O] = dpx match {
      case Empty               => out
      case g: FlatMap[I, w, X] => helper1(g.dpi, g.dpj(_: w).flatMap(f), out)
      case Read                => read[I, X].flatMap(f) <+> out
      case Sum(left, right)    => helper1(left, f, right.flatMap(f) <+> out)
      case Write(x)            => helper0(f(x), out)
    }

    def helper0(
        dpx: DParser[I, O],
        out: DParser[I, O]
    ): DParser[I, O] = dpx match {
      case f: FlatMap[I, x, O] => helper1(f.dpi, f.dpj, out)
      case Sum(left, right)    => helper0(left, right <+> out)
      case Empty               => out
      case _                   => dpx <+> out
    }

    helper0(dp, empty)
  }

  def read[I, O]: DParser[I, O] = Read

  def write[I, O](o: O): DParser[I, O] = Write(o)
  def pause[I]: DParser[I, Unit] = write(())
  def suspend[I, O](dpi: => DParser[I, O]): DParser[I, O] =
    pause[I].flatMap((_: Unit) => dpi)

  def empty[I, O]: DParser[I, O] = Empty

  implicit def isMonadPlus[I]
      : MonadPlus[({ type DPI[+O] = DParser[I, O] })#DPI] = {
    type DPI[+O] = DParser[I, O]
    new MonadPlus[DPI] {
      def bind[A, B](fa: DPI[A])(f: A => DPI[B]): DPI[B] = FlatMap(fa, f)
      def empty[A]: DPI[A] = Empty.asInstanceOf[DPI[A]]
      def plus[A](a: DPI[A], b: => DPI[A]): DPI[A] = Sum(a, b)
      def point[A](a: => A): DPI[A] = write(a)
    }
  }

  implicit def isMonoid[I, O]: Monoid[DParser[I, O]] = isMonadPlus[I].monoid

}
