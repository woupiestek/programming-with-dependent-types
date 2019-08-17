package nl.woupiestek.equalizer.parsing

import scalaz._

final case class ParserT[F[+ _], -I, +E, +A](
    derive: I => ParserT[F, I, E, A],
    errors: F[E],
    writes: F[A]
) {

  def flatMap[I0 <: I, E0 >: E, A0 >: A, B](
      f: A0 => ParserT[F, I0, E0, B]
  )(implicit F: MonadPlus[F]): ParserT[F, I0, E0, B] = {
    def h(pa: ParserT[F, I, E, A]): ParserT[F, I0, E0, B] =
      ParserT(
        (i: I0) =>
          h(pa.derive(i)).plus(
            pa.flatMap(f(_: A).derive(i))
          ),
        F.plus(
          pa.errors,
          F.bind(pa.writes)(f(_).errors)
        ),
        F.bind(pa.writes)(f(_).writes)
      )
    h(this)
  }

  def map[B](
      f: A => B
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, B] =
    copy(
      derive = derive(_).map(f),
      writes = F.map(writes)(f)
    )

  def filter(
      f: A => Boolean
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, A] =
    copy(
      derive = derive(_).filter(f),
      writes = F.filter(writes)(f)
    )

  def plus[I0 <: I, E0 >: E, A0 >: A](
      b: => ParserT[F, I0, E0, A0]
  )(implicit F: MonadPlus[F]): ParserT[F, I0, E0, A0] = {
    ParserT(
      (i: I0) => derive(i).plus(b.derive(i)),
      F.plus(errors, b.errors),
      F.plus(writes, b.writes)
    )
  }

  def ++[I0 <: I, E0 >: E, A0 >: A](
      b: => ParserT[F, I0, E0, A0]
  )(implicit F: MonadPlus[F]): ParserT[F, I0, E0, A0] =
    plus(b)
}

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
        def bind[A, B](fa: P[A])(f: A => P[B]): P[B] =
          fa.flatMap(f)
        def empty[A]: P[A] = Empty
        def plus[A](a: P[A], b: => P[A]): P[A] = a.plus(b)
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
    monadPlus[F, I, E]
      .empty[A]
      .copy(errors = F.point(e))
  def readIf[F[+ _], I, E](
      f: I => Boolean
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, I] =
    monadPlus[F, I, E]
      .empty[I]
      .copy(
        derive =
          (i: I) => if (f(i)) write(i) else monadPlus.empty
      )
  def write[F[+ _], I, E, A](
      a: => A
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, A] =
    monadPlus[F, I, E]
      .empty[A]
      .copy(writes = F.point(a))
}
