package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._

import scala.language.reflectiveCalls

final case class Rule[-I, +O](unfold: () => (I => Rule[I, O]) \/ (O, Rule[I, O]))

object Rule {

  def rule[I, O](r: => (I => Rule[I, O]) \/ (O, Rule[I, O])): Rule[I, O] =
    Rule(() => r)

  def ana[I, R, O](f: R => (I => R) \/ (O, R))(r: R): Rule[I, O] =
    rule(f(r).bimap(_ andThen ana(f), { case (o, s) => (o, ana(f)(s)) }))

  def fail[I, O]: Rule[I, O] = instance[I].empty[O]

  def emit[I, O](o: => O): Rule[I, O] = rule(\/-((o, fail)))

  def read[I]: Rule[I, I] = rule(-\/(emit(_)))

  def readIf[I](f: I => Boolean): Rule[I, I] = read[I].filter(f)

  implicit def instance[I]: MonadPlus[({type R[O] = Rule[I, O]})#R] =
    new MonadPlus[({type R[O] = Rule[I, O]})#R] {
      override def point[A](a: => A): Rule[I, A] = emit(a)

      override def bind[A, B](fa: Rule[I, A])(f: A => Rule[I, B]): Rule[I, B] =
        fa.unfold() match {
          case -\/(g) => rule(-\/((i: I) => bind(g(i))(f)))
          case \/-((a, b)) => plus(f(a), bind(b)(f))
        }

      override def empty[A]: Rule[I, A] = ana[I, Unit, A](r => -\/(_ => r))(())

      override def plus[A](a: Rule[I, A], b: => Rule[I, A]): Rule[I, A] =
        rule(a.unfold().fold(
          c => b.unfold().bimap(
            d => i => plus(c(i), d(i)),
            { case (d, e) => (d, plus(a, e)) }),
          { case (c, d) => \/-((c, plus(d, b))) }))
    }

  implicit class RuleOps[I, O](val rule: Rule[I, O]) extends AnyVal {

    def >[O2](other: Rule[I, O2]): Rule[I, O2] = (rule |@| other) ((_, x) => x)

    def <[O2](other: Rule[I, O2]): Rule[I, O] = (rule |@| other) ((x, _) => x)

    def |::|(tail: Rule[I,List[O]]): Rule[I, List[O]] = (rule |@| tail) (_ :: _)

    def oneOrMore: Rule[I, List[O]] = rule |::| rule.zeroOrMore

    def zeroOrMore: Rule[I, List[O]] = emit[I, List[O]](Nil) <+> rule.oneOrMore

    def zeroOrOne: Rule[I, Option[O]] = emit[I, Option[O]](None) <+> rule.map(Some(_))
  }

}