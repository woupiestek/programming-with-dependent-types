package nl.woupiestek.equalizer.parsing

import scalaz._

final case class ParserT[F[+ _], -I, +E, +O](
    val rules: F[ParserT.RuleT[F, I, E, O]]
) extends AnyVal {

  private def collect[Z](
      f: PartialFunction[ParserT.RuleT[F, I, E, O], Z]
  )(implicit F: MonadPlus[F]): F[Z] =
    F.bind(rules)(
      x => if (f.isDefinedAt(x)) F.point(f(x)) else F.empty
    )

  def matches(implicit F: MonadPlus[F]): F[O] =
    collect { case ParserT.Write(value) => value }

  def errors(implicit F: MonadPlus[F]): F[E] =
    collect { case ParserT.Error(value) => value }

  def derive(
      i: I
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, O] =
    ParserT(
      F.bind(rules) {
        case d: ParserT.Derive[F, I, E, O] =>
          d.derive(i).rules
        case _ => F.empty
      }
    )

}

object ParserT {

  sealed abstract class RuleT[F[+ _], -I, +E, +O]
  private final case class Write[F[+ _], A](
      value: A
  ) extends RuleT[F, Any, Nothing, A]

  private final case class Error[F[+ _], E](
      value: E
  ) extends RuleT[F, Any, E, Nothing]

  private final case class Derive[F[+ _]: MonadPlus, I, E, A](
      derive: I => ParserT[F, I, E, A]
  ) extends RuleT[F, I, E, A]

  private final class Instances[F[+ _], I, E](
      implicit F: MonadPlus[F]
  ) {
    type P[+O] = ParserT[F, I, E, O]

    implicit val monadPlus: MonadPlus[P] =
      new MonadPlus[P] {
        def bind[A, B](fa: P[A])(f: A => P[B]): P[B] = {
          def h(pa: P[A]): P[B] =
            ParserT(F.bind(pa.rules) {
              case Write(value) => f(value).rules
              case Error(_)     => F.empty
              case Derive(d) =>
                F.point(Derive(d andThen h))
            })
          h(fa)
        }
        def empty[A]: P[A] = ParserT(F.empty)
        def plus[A](a: P[A], b: => P[A]): P[A] =
          ParserT(F.plus(a.rules, b.rules))
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
    parser(new Error[F, E](e))
  def read[F[+ _]: MonadPlus, I, E]: ParserT[F, I, E, I] =
    parser(Derive[F, I, E, I](monadPlus.point(_: I)))
  def readIf[F[+ _]: MonadPlus, I, E](
      f: I => Boolean
  ): ParserT[F, I, E, I] =
    monadPlus.filter(read[F, I, E])(f)
  def write[F[+ _]: MonadPlus, I, E, A](
      a: => A
  ): ParserT[F, I, E, A] =
    parser(new Write[F, A](a))

  private def parser[F[+ _]: MonadPlus, I, E, A](
      r: => RuleT[F, I, E, A]
  ): ParserT[F, I, E, A] =
    ParserT(MonadPlus[F].point(r))

}
