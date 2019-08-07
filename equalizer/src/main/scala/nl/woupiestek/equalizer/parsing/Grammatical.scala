package nl.woupiestek.equalizer.parsing

import scalaz._

trait Grammatical[P[- _, + _]] {
  implicit def monadPlus[A]: ApplicativePlus[({ type F[B] = P[A, B] })#F]
  implicit def foldable[A]: Foldable[({ type F[B] = (A, P[A, B]) })#F]

  //some laws...
  def ask[A, B](f: A => P[A, B]): P[A, B]
}

object Grammatical {
  def apply[P[- _, + _]](implicit P: Grammatical[P]) = P
}
