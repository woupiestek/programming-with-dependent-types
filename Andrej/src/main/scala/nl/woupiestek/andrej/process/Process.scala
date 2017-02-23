package nl.woupiestek.andrej.process

sealed trait Process[I, O]

case class Emit[I, O](out: O, next: Process[I, O]) extends Process[I, O]

case class Await[I, O](next: I => Process[I, O]) extends Process[I, O]

object Process {
  def identity[X]: Process[X, X] = Await(Emit(_, identity))

  def compose[X, Y, Z](f: Process[Y, Z], g: Process[X, Y]): Process[X, Z] = (f, g) match {
    case (Emit(x, y), z) => Emit(x, compose(y, z))
    case (Await(x), Emit(y, z)) => compose(x(y), z)
    case (x, Await(y)) => Await(i => compose(x, y(i)))
  }
}
