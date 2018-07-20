package nl.woupiestek.andrej.equalambda.attempt2

trait TermWithQ[T] extends Term[T] {
  /* |-M=N
   * |-P:M=N->p
   * ----------
   * |-M=N?P:p
   */
  def equalizer(left: T, right: T, term: T): T

  /* |-M=N,
   * M=N|-P:p
   * --------------
   * |-M=N!P:M=N->p
   */
  def factor(left: T, right: T, term: T): T
}
