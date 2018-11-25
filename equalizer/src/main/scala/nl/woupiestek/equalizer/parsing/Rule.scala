package nl.woupiestek.equalizer.parsing

import shapeless._

case class Rule[I, O](next: I => Rule[I, O], done: Seq[O])

object Rule {

  def read[I]: Rule[I, I] = Rule(unit, Seq.empty)

  def readIf[I](f: I => Boolean): Rule[I, I] = read[I].filter(f)

  def joinAll[I, O](rules: Seq[Rule[I, O]]): Rule[I, O] =
    Rule((i: I) => joinAll(rules.map(_.next(i))), rules.flatMap(_.done))

  def join[I, O](x: Rule[I, O]*): Rule[I, O] = joinAll(x)

  def unit[I, O](o: O): Rule[I, O] = Rule((_: I) => join(), List(o))

  def nil[I, O]: Rule[I, List[O]] = unit(Nil)

  def sequence[I, O](rules: List[Rule[I, O]]): Rule[I, List[O]] =
    traverse(rules)(r => r)

  def traverse[I, A, B](list: List[A])(f: A => Rule[I, B]): Rule[I, List[B]] =
    list.foldRight(nil[I, B])((h,t) => f(h) ::: t)

  implicit class MonadOps[I, O](rule: Rule[I, O]) {
    def before[O2](f: Seq[O] => Seq[O2]): Rule[I, O2] =
      Rule(rule.next(_).before(f), f(rule.done))

    def flatMap[O2](f: O => Rule[I, O2]): Rule[I, O2] = join(
      Rule(rule.next(_: I).flatMap(f), Seq.empty),
      joinAll(rule.done.map(f).toList))

    def map[O2](f: O => O2): Rule[I, O2] = before(_.map(f))

    def withFilter(f: O => Boolean): Rule[I, O] = before(_.filter(f))

    def filter(f: O => Boolean): Rule[I, O] = withFilter(f)

    def collect[O2](f: PartialFunction[O, O2]): Rule[I, O2] =
      before(_.collect(f))

    def ignore: Rule[I, Unit] = before(_ => Seq(()))

    def zip[O2, O3](rule2: Rule[I, O2])(f: (O, O2) => O3): Rule[I, O3] =
      rule.flatMap(x => rule2.map(f(x, _)))

    def par[O2, O3](rule2: Rule[I, O2])(f: (O, O2) => O3): Rule[I, O3] =
      (rule split rule2) ((a, b) => a.flatMap(c => b.map(d => f(c, d))))

    def >[O2](other: Rule[I, O2]): Rule[I, O2] = zip(other)((_, x) => x)

    def <[O2](other: Rule[I, O2]): Rule[I, O] = zip(other)((x, _) => x)

    def oneOrMore: Rule[I, List[O]] = rule ::: rule.zeroOrMore

    def zeroOrMore: Rule[I, List[O]] = join(nil, rule.oneOrMore)

    def zeroOrOne: Rule[I, Option[O]] = join(unit(None), rule.map(Some(_)))

    def split[A, B](
      ruleA: Rule[I, A])(
      combine: (Seq[O], Seq[A]) => Seq[B]): Rule[I, B] = Rule(
      (i: I) => rule.next(i).split(ruleA.next(i))(combine),
      combine(rule.done, ruleA.done))

    def or[O2](rule2: Rule[I, O2]): Rule[I, Either[O, O2]] =
      split(rule2)((a, b) => a.map(Left(_)) ++ b.map(Right(_)))

    def $: Rule[I, O :: HNil] = map(_ :: HNil)

    def ~[T <: HList](rule2: Rule[I, T]): Rule[I, O :: T] =
      (rule zip rule2) (_ :: _)
  }

  implicit class ListOps[I, O](rule: Rule[I, List[O]]) {
    def :::(rule2: Rule[I, O]): Rule[I, List[O]] = (rule2 zip rule) (_ :: _)
  }

}