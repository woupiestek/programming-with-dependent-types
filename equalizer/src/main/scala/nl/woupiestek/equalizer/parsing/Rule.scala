package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.language.reflectiveCalls

case class Rule[I, O](next: I => Rule[I, O], done: Seq[O])

object Rule {

  def read[I]: Rule[I, I] = Rule(unit, Seq.empty)

  def readIf[I](f: I => Boolean): Rule[I, I] = read[I].filter(f)

  def joinAll[I, O](rules: Seq[Rule[I, O]]): Rule[I, O] =
    Rule((i: I) => joinAll(rules.map(_.next(i))), rules.flatMap(_.done))

  def or[I, O](x: Rule[I, O]*): Rule[I, O] = joinAll(x)

  def unit[I, O](o: O): Rule[I, O] = Rule((_: I) => or(), List(o))

  def nil[I, O]: Rule[I, List[O]] = unit(Nil)

  def sequence[I, O](rules: List[Rule[I, O]]): Rule[I, List[O]] =
    traverse(rules)(r => r)

  def traverse[I, A, B](list: List[A])(f: A => Rule[I, B]): Rule[I, List[B]] =
    list.foldRight(nil[I, B])((h, t) => f(h) ::: t)

  implicit def instance[I]: Monad[({type R[O] = Rule[I, O]})#R] =
    new Monad[({type R[O] = Rule[I, O]})#R] {
      override def point[A](a: => A): Rule[I, A] = unit(a)

      override def bind[A, B](fa: Rule[I, A])(f: A => Rule[I, B]): Rule[I, B] =
        or(
          Rule((i: I) => bind(fa.next(i))(f), Seq.empty),
          joinAll(fa.done.map(f)))
    }

  implicit class MonadOps[I, O](rule: Rule[I, O]) {

    def before[O2](f: Seq[O] => Seq[O2]): Rule[I, O2] =
      Rule(rule.next(_).before(f), f(rule.done))

    def withFilter(f: O => Boolean): Rule[I, O] = before(_.filter(f))

    def filter(f: O => Boolean): Rule[I, O] = withFilter(f)

    def collect[O2](f: PartialFunction[O, O2]): Rule[I, O2] =
      before(_.collect(f))

    def ignore: Rule[I, Unit] = before(_ => Seq(()))

    def zip[O2, O3](rule2: Rule[I, O2])(f: (O, O2) => O3): Rule[I, O3] =
      (rule |@| rule2) (f)

    def par[O2, O3](rule2: Rule[I, O2])(f: (O, O2) => O3): Rule[I, O3] =
      (rule split rule2) ((a, b) => a.flatMap(c => b.map(d => f(c, d))))

    def >[O2](other: Rule[I, O2]): Rule[I, O2] = (rule |@| other) ((_, x) => x)

    def <[O2](other: Rule[I, O2]): Rule[I, O] = (rule |@| other) ((x, _) => x)

    def oneOrMore: Rule[I, List[O]] = rule ::: rule.zeroOrMore

    def zeroOrMore: Rule[I, List[O]] = or(nil, rule.oneOrMore)

    def zeroOrOne: Rule[I, Option[O]] = or(unit(None), rule.map(Some(_)))

    def split[A, B](
      ruleA: Rule[I, A])(
      combine: (Seq[O], Seq[A]) => Seq[B]): Rule[I, B] = Rule(
      (i: I) => rule.next(i).split(ruleA.next(i))(combine),
      combine(rule.done, ruleA.done))

  }

  implicit class ListOps[I, O](rule: Rule[I, List[O]]) {
    def :::(rule2: Rule[I, O]): Rule[I, List[O]] = (rule2 |@| rule) (_ :: _)
  }

}