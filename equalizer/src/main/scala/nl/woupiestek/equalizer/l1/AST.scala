package nl.woupiestek.equalizer.l1

object AST {

  trait Cut[V] {
    def variable(name: String): V
    def let(name: String, value: V, context: V): V
  }

  trait Type[T] extends Cut[T] {
    def arrow(source: T, target: T): T
    def fix(name: String, context: T): T
    def product(types: List[T]): T
  }

  trait Def[D] extends Cut[D] {
    def application(operator: D, operand: D): D
    def abstraction(name: String, body: D): D
    def tuple(defs: List[D]): D
    def project(d: D, index: Int): D
    def fix(name: String, body: D): D
    def unfold(body: D): D
  }

}
