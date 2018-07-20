package nl.woupiestek.andrej.equalambda.attempt2

abstract class Type[Ty, Te: Term] {

  def eq(left: Te, right: Te): Ty

  def product(name: String, dom: Ty, cod: Ty)

  def variable(index: Int): Ty

}
