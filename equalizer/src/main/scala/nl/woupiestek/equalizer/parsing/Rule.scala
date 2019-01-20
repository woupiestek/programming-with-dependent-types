package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.language.{higherKinds, reflectiveCalls}

trait Rule[R[_, _], I] extends ApplicativePlus[({type RI[O] = R[I, O]})#RI] {
  def readIf(f: I => Boolean): R[I, I]

  type RI[O] = R[I, O]
}

object Rule {

  implicit class ops[R[_, _], I, O](rule: R[I, O])(implicit R: Rule[R, I]) {

    def >[O2](other: R[I, O2]): R[I, O2] = (rule |@| other) ((_, x) => x)

    def <[O2](other: R[I, O2]): R[I, O] = (rule |@| other) ((x, _) => x)

    def |::|(tail: R[I, List[O]]): R[I, List[O]] = (rule |@| tail) (_ :: _)

    def oneOrMore: R[I, List[O]] = rule |::| rule.zeroOrMore

    def zeroOrMore: R[I, List[O]] = rule.oneOrMore <+> List.empty[O].point[R.RI]

    def zeroOrOne: R[I, Option[O]] =
      rule.map(_.point[Option]) <+> Option.empty[O].point[R.RI]
  }

}