package nl.woupiestek.andrej.lispy

import scala.language.higherKinds

trait Lisp[L] {

  def nil: L

  def elt(name: String): L

  def cons(h: L, t: L): L

  def quote(l: L): L

  def atom(l: L): Boolean

  def eq(x: L, y: L): Boolean

  def car(l: L): Option[L]

  def cdr(l: L): Option[L]

  def cond(x: => List[(Boolean, L)]): L

  def lambda(f: L => L): L

}

trait Levy[C[_], T[_]] {

  def ret[X](x: X): C[X]

  def bind[X, Y](x: C[X])(f: C[X => Y]): C[Y]

  def lambda[X, Y](f: X => C[Y]): C[X => Y]

  def alpha[X, Y](x: X, f: C[X => Y]): C[Y]

  def thunk[X](x: C[X]): T[X]

  def force[X](x: T[X]): C[X]

  //...
}
