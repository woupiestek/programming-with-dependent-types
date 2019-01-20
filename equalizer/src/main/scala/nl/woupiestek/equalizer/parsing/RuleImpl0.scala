package nl.woupiestek.equalizer.parsing

import scalaz._

import scala.language.reflectiveCalls

final case class RuleImpl0[-I, +O](unfold: () => (I => RuleImpl0[I, O]) \/ (O, RuleImpl0[I, O]))

object RuleImpl0 {

  def rule[I, O](r: => (I => RuleImpl0[I, O]) \/ (O, RuleImpl0[I, O])): RuleImpl0[I, O] =
    RuleImpl0(() => r)

  def ana[I, R, O](f: R => (I => R) \/ (O, R))(r: R): RuleImpl0[I, O] =
    rule(f(r).bimap(_ andThen ana(f), { case (o, s) => (o, ana(f)(s)) }))

  implicit def instance[I]: Rule[RuleImpl0, I] = new Rule[RuleImpl0, I] {

    override def ap[A, B](fa: => RuleImpl0[I, A])(f: => RuleImpl0[I, A => B]): RuleImpl0[I, B] =
      rule(fa.unfold() match {
        case -\/(a) => -\/((i: I) => ap(a(i))(f))
        case \/-((a0, a1)) => f.unfold() match {
          case -\/(b) => -\/((i: I) => ap(fa)(b(i)))
          case \/-((b0, b1)) => \/-((b0(a0), ap(a1)(b1)))
        }
      })

    override def point[A](a: => A): RuleImpl0[I, A] =  rule(\/-((a, empty)))

    override def empty[A]: RuleImpl0[I, A] =
      ana[I, Unit, A](r => -\/(_ => r))(())

    override def plus[A](a: RuleImpl0[I, A], b: => RuleImpl0[I, A]): RuleImpl0[I, A] =
      rule(a.unfold().fold(
        c => b.unfold().bimap(
          d => i => plus(c(i), d(i)),
          { case (d, e) => (d, plus(a, e)) }),
        { case (c, d) => \/-((c, plus(d, b))) }))

    override def readIf(f: I => Boolean): RuleImpl0[I, I] =
      rule(-\/((i: I) => if (f(i)) point(i) else empty))
  }

}