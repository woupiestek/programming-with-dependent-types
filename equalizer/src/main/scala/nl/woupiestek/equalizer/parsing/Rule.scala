package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.language.higherKinds

trait Rule[R[_], I] {
  implicit def isApplicativePlus: ApplicativePlus[R]
  //parsing has become traversing
  def lift[O](f: I => R[O]): R[O]
}

object Rule {

  implicit class ops[R[_], I, O](rule: R[O])(implicit R: ApplicativePlus[R]) {
    def nel: R[NonEmptyList[O]] = (rule |@| rule.list)(NonEmptyList.nel)

    def list: R[IList[O]] = rule.nel.map(_.list) <+> IList.empty[O].point[R]

    def maybe: R[Maybe[O]] =
      rule.map(_.point[Maybe]) <+> Maybe.empty[O].point[R]

    def oneOrMore(implicit O: Semigroup[O]): R[O] =
      rule <+> (rule |@| oneOrMore)(O.append(_, _))

    def zeroOrMore(implicit O: Monoid[O]): R[O] = oneOrMore <+> O.zero.pure[R]
  }

  def readIf[R[_], A](f: A => Boolean)(implicit R: Rule[R, A]): R[A] = {
    import R._
    lift((i: A) => if (f(i)) i.point[R] else R.isApplicativePlus.empty)
  }

  def readWhile[R[_], A, B](
      f: A => Boolean
  )(g: A => B)(implicit R: Rule[R, A], B: Monoid[B]): R[B] = {
    import R._
    (readIf(f).map(g) |@| readWhile(f)(g))(B.append(_, _)) <+> B.zero.pure[R]
  }

}
