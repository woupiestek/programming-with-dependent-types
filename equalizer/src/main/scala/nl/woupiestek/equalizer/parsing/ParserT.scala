package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.collection.mutable

final case class ParserT[F[+ _], -I, +E, +O](
    val rules: F[ParserT.RuleT[F, I, E, O]]
) extends AnyVal {

  private def _fold[Z](
      f: PartialFunction[ParserT.RuleT[F, I, E, O], F[Z]]
  )(implicit F: MonadPlus[F]): F[Z] =
    rules.flatMap { rule =>
      if (f.isDefinedAt(rule)) f(rule)
      else if (rule.isInstanceOf[
                 ParserT.Suspend[F, I, E, O]
               ])
        rule
          .asInstanceOf[ParserT.Suspend[F, I, E, O]]
          .value
          ._fold(f)
      else F.empty
    }

  def matches(implicit F: MonadPlus[F]): F[O] =
    _fold {
      case rule if rule.isInstanceOf[ParserT.Write[F, O]] =>
        rule
          .asInstanceOf[ParserT.Write[F, O]]
          .value
          .point[F]
    }

  def errors(implicit F: MonadPlus[F]): F[E] =
    _fold {
      case rule if rule.isInstanceOf[ParserT.Error[F, E]] =>
        rule
          .asInstanceOf[ParserT.Error[F, E]]
          .value
          .point[F]
    }

  def derive(
      i: I
  )(implicit F: MonadPlus[F]): ParserT[F, I, E, O] = {
    ParserT(
      _fold {
        case rule
            if rule
              .isInstanceOf[ParserT.Derive[F, I, E, O]] =>
          rule
            .asInstanceOf[ParserT.Derive[F, I, E, O]]
            .derive(i)
            .rules
      }
    )
  }
}

object ParserT {

  sealed abstract class RuleT[F[+ _]: MonadPlus, -I, +E, +O] {
    final def asParser[I0 <: I, E0 >: E, O0 >: O]
        : ParserT[F, I0, E0, O0] =
      ParserT(this.point[F])
  }

  private final class Write[F[+ _]: MonadPlus, A](
      val value: A
  ) extends RuleT[F, Any, Nothing, A]

  private final class Error[F[+ _]: MonadPlus, E](
      val value: E
  ) extends RuleT[F, Any, E, Nothing]

  private final class Derive[F[+ _]: MonadPlus, I, E, A](
      d: I => ParserT[F, I, E, A]
  ) extends RuleT[F, I, E, A] {
    private val cache =
      new mutable.HashMap[I, ParserT[F, I, E, A]]
    def derive(i: I): ParserT[F, I, E, A] = {
      if (!cache.contains(i)) {
        cache.put(i, d(i))
      }
      cache(i)
    }
  }

  private final class Suspend[F[+ _]: MonadPlus, I, E, A](
      p: => ParserT[F, I, E, A]
  ) extends RuleT[F, I, E, A] {
    lazy val value: ParserT[F, I, E, A] = p
  }

  private final class Instances[F[+ _]: MonadPlus, I, E] {
    type P[+O] = ParserT[F, I, E, O]

    implicit val monadPlus: MonadPlus[P] =
      new MonadPlus[P] {
        def bind[A, B](fa: P[A])(f: A => P[B]): P[B] =
          ParserT(
            fa.matches.flatMap(f(_).rules) <+>
              new Derive((i: I) => bind(fa.derive(i))(f))
                .point[F]
          )
        def empty[A]: P[A] = ParserT(PlusEmpty[F].empty)
        def plus[A](a: P[A], b: => P[A]): P[A] =
          ParserT(
            a.rules <+> new Suspend[F, I, E, A](b).point[F]
          )
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
    suspend(new Error[F, E](e).asParser[I, E, A])
  def read[F[+ _]: MonadPlus, I, E]: ParserT[F, I, E, I] =
    new Derive[F, I, E, I](
      (_: I).point[({ type P[+O] = ParserT[F, I, E, O] })#P]
    ).asParser[I, E, I]
  def readIf[F[+ _]: MonadPlus, I, E](
      f: I => Boolean
  ): ParserT[F, I, E, I] = read[F, I, E].filter(f)
  def suspend[F[+ _]: MonadPlus, I, E, A](
      p: => ParserT[F, I, E, A]
  ): ParserT[F, I, E, A] =
    new Suspend[F, I, E, A](p).asParser[I, E, A]
  def write[F[+ _]: MonadPlus, I, E, A](
      a: => A
  ): ParserT[F, I, E, A] =
    suspend(new Write[F, A](a).asParser[I, E, A])
}
