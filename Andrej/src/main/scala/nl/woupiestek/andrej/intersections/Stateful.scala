package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.Stateful.point

case class Stateful[S, T](go: S => (T, S)) {
  def flatMap[U](f: T => Stateful[S, U]): Stateful[S, U] = Stateful(go andThen { case (y, z) => f(y).go(z) })

  def map[U](g: T => U): Stateful[S, U] = flatMap(g andThen point)
}

object Stateful {
  def point[S, T](t: T): Stateful[S, T] = Stateful(s => (t, s))

  def state[S]: Stateful[S, S] = Stateful[S, S](s => (s, s))
}

//comprehensible (flatMap && map)
//filter <-> pointed