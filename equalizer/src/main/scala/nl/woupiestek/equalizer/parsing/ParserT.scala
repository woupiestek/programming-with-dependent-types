package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

final case class ParserT[F[+ _], -I, +E, +O](
    val rules: F[ParserT.RuleT[F, I, E, O]]
) extends AnyVal {

  def matches(implicit F: MonadPlus[F]): F[O] =
    rules
      .filter(_.isInstanceOf[ParserT.Write[F, O]])
      .map(_.asInstanceOf[ParserT.Write[F, O]].value)

  def errors(implicit F: MonadPlus[F]): F[E] =
    rules
      .filter(_.isInstanceOf[ParserT.Error[F, E]])
      .map(_.asInstanceOf[ParserT.Error[F, E]].value)

  def derive(
      i: I
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, O] =
    ParserT(rules.flatMap(_.derive(i).rules))
}

object ParserT {

  sealed abstract class RuleT[F[+ _]: MonadPlus, -I, +E, +O] {
    def derive(i: I): ParserT[F, I, E, O]
    def bind[P, I0 <: I, E0 >: E](
        f: O => ParserT[F, I0, E0, P]
    ): ParserT[F, I0, E0, P]
    final def asParser[I0 <: I, E0 >: E, O0 >: O]
        : ParserT[F, I0, E0, O0] =
      ParserT(this.point[F])
  }

  private final class Write[F[+ _]: MonadPlus, A](a: => A)
      extends RuleT[F, Any, Nothing, A] {
    def bind[B, I0, E0](
        f: A => ParserT[F, I0, E0, B]
    ): ParserT[F, I0, E0, B] = f(value)
    lazy val value: A = a
    def derive(i: Any): ParserT[F, Any, Nothing, A] =
      ParserT(PlusEmpty[F].empty)
  }

  private final class Error[F[+ _]: MonadPlus, E](e: => E)
      extends RuleT[F, Any, E, Nothing] {
    def bind[B, I0, E0 >: E](
        f: Nothing => ParserT[F, I0, E0, B]
    ): ParserT[F, I0, E0, B] = ParserT(this.point[F])
    lazy val value: E = e
    def derive(i: Any): ParserT[F, Any, E, Nothing] =
      ParserT(PlusEmpty[F].empty)
  }

  private final class Derive[F[+ _]: MonadPlus, -I, +E, +A](
      d: => I => ParserT[F, I, E, A]
  ) extends RuleT[F, I, E, A] {
    def bind[B, I0 <: I, E0 >: E](
        f: A => ParserT[F, I0, E0, B]
    ): ParserT[F, I0, E0, B] =
      new Derive(
        d(_: I0)
          .asInstanceOf[ParserT[F, I0, E0, A]]
          .flatMap(f)
      ).asParser[I0, E0, B]
    def derive(i: I) = d(i)
  }

  private final class Instances[F[+ _]: MonadPlus, I, E] {
    type P[+O] = ParserT[F, I, E, O]

    implicit val monadPlus: MonadPlus[P] =
      new MonadPlus[P] {
        def bind[A, B](fa: P[A])(f: A => P[B]): P[B] =
          ParserT(fa.rules.flatMap(_.bind(f).rules))
        def empty[A]: P[A] = ParserT(PlusEmpty[F].empty)
        def plus[A](a: P[A], b: => P[A]): P[A] =
          ParserT(a.rules <+> b.rules)
        def point[A](a: => A): P[A] = write(a)
      }
  }
  implicit def monadPlus[F[+ _]: MonadPlus, I, E]
      : MonadPlus[
        ({ type P[+O] = ParserT[F, I, E, O] })#P
      ] =
    new Instances[F, I, E].monadPlus

  def error[F[+ _]: MonadPlus, I, E, A](
      e: => E
  ): ParserT[F, I, E, A] =
    new Error[F, E](e).asParser[I, E, A]
  def write[F[+ _]: MonadPlus, I, E, A](
      a: => A
  ): ParserT[F, I, E, A] =
    new Write[F, A](a).asParser[I, E, A]
  def read[F[+ _]: MonadPlus, I, E]: ParserT[F, I, E, I] =
    new Derive[F, I, E, I](
      (_: I).point[({ type P[+O] = ParserT[F, I, E, O] })#P]
    ).asParser[I, E, I]
  def readIf[F[+ _]: MonadPlus, I, E](
      f: I => Boolean
  ): ParserT[F, I, E, I] =
    read[F, I, E].filter(f)
}
