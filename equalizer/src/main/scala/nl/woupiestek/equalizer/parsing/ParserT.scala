package nl.woupiestek.equalizer.parsing

import scalaz._

final case class ParserT[F[+ _], -I, +E, +O](
    derive: I => ParserT[F, I, E, O],
    errors: F[E],
    writes: F[O]
)

object ParserT {

  private final class Instances[F[+ _], I, E](
      implicit F: MonadPlus[F]
  ) {
    type P[+O] = ParserT[F, I, E, O]
    private lazy val Empty: P[Nothing] = ParserT(
      (_: I) => Empty,
      F.empty,
      F.empty
    )

    implicit val monadPlus: MonadPlus[P] =
      new MonadPlus[P] {
        def bind[A, B](fa: P[A])(f: A => P[B]): P[B] = {
          def h(pa: P[A]): P[B] =
            ParserT(
              (i: I) => h(pa.derive(i)),
              pa.errors,
              F.bind(pa.writes)(f(_).writes)
            )
          h(fa)
        }
        def empty[A]: P[A] = Empty
        def plus[A](a: P[A], b: => P[A]): P[A] =
          ParserT(
            (i: I) => plus(a.derive(i), b.derive(i)),
            F.plus(a.errors, b.errors),
            F.plus(a.writes, b.writes)
          )
        def point[A](a: => A): P[A] = write(a)
      }
  }
  implicit def monadPlus[F[+ _]: MonadPlus, I, E]
      : MonadPlus[
        ({ type P[+O] = ParserT[F, I, E, O] })#P
      ] =
    new Instances[F, I, E].monadPlus

  def error[F[+ _], I, E, A](
      e: => E
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, A] =
    ParserT(_ => monadPlus.empty, F.point(e), F.empty)
  def readIf[F[+ _], I, E](
      f: I => Boolean
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, I] =
    ParserT(
      (i: I) =>
        if (f(i)) monadPlus.point(i) else monadPlus.empty,
      F.empty,
      F.empty
    )
  def write[F[+ _], I, E, A](
      a: => A
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, A] =
    ParserT(_ => monadPlus.empty, F.empty, F.point(a))
}
