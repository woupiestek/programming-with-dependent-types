package nl.woupiestek.andrej.equalambda.attempt2

trait Term[T] {
  def identifier(name: String): T

  def where(term: T, substitution: Map[String, T]): T

  def application(operator: T, operands: List[T]): T

  def abstraction(arg: String, body: T): T
}
