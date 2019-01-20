package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.language.{higherKinds, reflectiveCalls}

trait Rule[R[_], I] extends ApplicativePlus[R] {
  def readIf(f: I => Boolean): R[I]
}

object Rule {

  implicit class ops[R[_], I, O](rule: R[O])(implicit R: Rule[R, I]) {

    def >[O2](other: R[O2]): R[O2] = (rule |@| other) ((_, x) => x)

    def <[O2](other: R[O2]): R[O] = (rule |@| other) ((x, _) => x)

    def |::|(tail: R[List[O]]): R[List[O]] = (rule |@| tail) (_ :: _)

    def oneOrMore: R[List[O]] = rule |::| rule.zeroOrMore

    def zeroOrMore: R[List[O]] = rule.oneOrMore <+> List.empty[O].point[R]

    def zeroOrOne: R[Option[O]] =
      rule.map(_.point[Option]) <+> Option.empty[O].point[R]
  }

}