package nl.woupiestek.andrej.equalambda

trait TermWithQ[T] extends Term[T] {
  /* |-M=N
   * |-P:M=N->p
   * ----------
   * |-M=N?P:p
   */
  def check(left: T, right: T, term: T): T

  /* |-M=N,
   * M=N|-P:p
   * --------------
   * |-M=N!P:M=N->p
   */
  def unify(left: T, right: T, term: T): T
}

trait NumVar[T] {
  def param(int: Int): T
}