package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

case class ParserT[F[+ _], -I, +E, +O](val rules: F[ParserT.Rule[F, I, E, O]])
    extends AnyVal

object ParserT {
  sealed abstract class Rule[F[+ _], -I, +E, +O]
  final case class Read[F[+ _], -I, +E, +O](f: I => ParserT[F, I, E, O])
      extends Rule[F, I, E, O]
  final case class Write[F[+ _], +O](o: O) extends Rule[F, Any, Nothing, O]
  final case class Error[F[+ _], +E](e: E) extends Rule[F, Any, E, Nothing]

  class Instances[F[+ _]: MonadPlus, I, E] {
    type P[+O] = ParserT[F, I, E, O]
    implicit val monadPlus: MonadPlus[P] = new MonadPlus[P] {
      def bind[A, B](fa: P[A])(f: A => P[B]): P[B] =
        ParserT(fa.rules.flatMap {
          case Error(e) => Error(e).point[F]
          case Read(g)  => Read(g(_: I).flatMap(f)).point[F]
          case Write(a) => f(a).rules
        })
      def empty[A]: P[A] = ParserT(PlusEmpty[F].empty)
      def plus[A](a: P[A], b: => P[A]): P[A] =
        ParserT(PlusEmpty[F].plus(a.rules, b.rules))
      def point[A](a: => A): P[A] = ParserT(Applicative[F].point(Write(a)))
    }
  }

  implicit def monadPlus[F[+ _]: MonadPlus, I, E] =
    new Instances[F, I, E].monadPlus

  def fromRule[F[+ _]: MonadPlus, I, E, O](rule: => Rule[F, I, E, O]) = {
    ParserT(rule.point[F])
  }

  def error[F[+ _]: MonadPlus, I, E](e: E): ParserT[F, I, E, E] = {
    fromRule(Error(e))
  }

  def read[F[+ _]: MonadPlus, I, E]: ParserT[F, I, E, I] = {
    type P[+O] = ParserT[F, I, E, O]
    fromRule(Read((_: I).point[P]))
  }

  def readIf[F[+ _]: MonadPlus, I, E](
      f: I => Boolean
  ): ParserT[F, I, E, I] = read[F, I, E].filter(f)
}
