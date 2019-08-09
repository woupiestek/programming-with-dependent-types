package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

final case class ParserT[F[+ _], -I, +E, +O](
    val rules: F[ParserT.RuleT[F, I, E, O]]
) extends AnyVal

object ParserT {

  sealed abstract class RuleT[F[+ _]: MonadPlus, -I, +E, +O] {
    def derive(i: I): ParserT[F, I, E, O]
    def bind[P, I0 <: I, E0 >: E](
        f: O => ParserT[F, I0, E0, P]
    ): ParserT[F, I0, E0, P]
  }

  private final class Write[F[+ _]: MonadPlus, A](a: => A)
      extends RuleT[F, Any, Nothing, A] {
    def bind[B, I0, E0](
        f: A => ParserT[F, I0, E0, B]
    ): ParserT[F, I0, E0, B] = f(a)
    def value: A = a
    def derive(i: Any): ParserT[F, Any, Nothing, A] =
      ParserT(PlusEmpty[F].empty)
  }

  private final class Error[F[+ _]: MonadPlus, E](e: => E)
      extends RuleT[F, Any, E, Nothing] {
    def bind[B, I0, E0 >: E](
        f: Nothing => ParserT[F, I0, E0, B]
    ): ParserT[F, I0, E0, B] = ParserT(this.point[F])
    def value: E = e
    def derive(i: Any): ParserT[F, Any, E, Nothing] =
      ParserT(PlusEmpty[F].empty)
  }

  private final class Read[F[+ _]: MonadPlus, -I, +E, +A](
      select: I => ParserT[F, I, E, A]
  ) extends RuleT[F, I, E, A] {
    def bind[B, I0 <: I, E0 >: E](
        f: A => ParserT[F, I0, E0, B]
    ): ParserT[F, I0, E0, B] =
      read[F, I0, E0, B](
        select(_: I0).asInstanceOf[ParserT[F, I0, E0, A]].flatMap(f)
      )
    def derive(i: I): ParserT[F, I, E, A] =
      select(i)
  }

  private final class Instances[F[+ _]: MonadPlus, I, E] {
    type P[+O] = ParserT[F, I, E, O]

    implicit val monadPlus: MonadPlus[P] = new MonadPlus[P] {
      def bind[A, B](fa: P[A])(f: A => P[B]): P[B] =
        ParserT(fa.rules.flatMap(_.bind(f).rules))
      def empty[A]: P[A] = ParserT(PlusEmpty[F].empty)
      def plus[A](a: P[A], b: => P[A]): P[A] = ParserT(a.rules <+> b.rules)
      def point[A](a: => A): P[A] = write(a)
    }
  }
  implicit def monadPlus[F[+ _]: MonadPlus, I, E]
      : MonadPlus[({ type P[+O] = ParserT[F, I, E, O] })#P] =
    new Instances[F, I, E].monadPlus

  def read[F[+ _]: MonadPlus, I, E, A](
      select: I => ParserT[F, I, E, A]
  ): ParserT[F, I, E, A] =
    ParserT(
      new Read[F, I, E, A](select)
        .asInstanceOf[RuleT[F, I, E, A]]
        .point[F]
    )
  def error[F[+ _]: MonadPlus, I, E, A](e: => E): ParserT[F, I, E, A] =
    ParserT(new Error[F, E](e).asInstanceOf[RuleT[F, I, E, A]].point[F])
  def write[F[+ _]: MonadPlus, I, E, A](a: => A): ParserT[F, I, E, A] =
    ParserT(new Write[F, A](a).point[F])
}
