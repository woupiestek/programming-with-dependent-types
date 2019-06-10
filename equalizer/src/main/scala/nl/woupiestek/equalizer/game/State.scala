package nl.woupiestek.equalizer.game

import State._

sealed abstract class State[I, A] {

  @annotation.tailrec
  def run(state: I): A = this match {
    case Get(f)    => f(state).run(state)
    case Set(i, a) => a.run(i)
    case Don(a)    => a
    case Con(a, f) =>
      a match {
        case Get(g)    => g(state).flatMap(f).run(state)
        case Set(j, b) => b.flatMap(f).run(j)
        case Con(b, g) => b.flatMap(g(_).flatMap(f)).run(state)
        case Don(b)    => f(b).run(state)
      }
  }

  def flatMap[B](
      f: A => State[I, B]
  ): State[I, B] = this match {
    case con: Con[i, c, b] => Con(con.a, con.f(_: c).flatMap(f))
    case Don(a)            => f(a)
    case Get(g)            => Get(g(_).flatMap(f))
    case Set(i, a)         => Set(i, Con(a, f))
  }
}

object State {
  private final case class Get[I, A](f: I => State[I, A]) extends State[I, A]
  private final case class Set[I, A](i: I, a: State[I, A]) extends State[I, A]
  private final case class Con[I, A, B](
      a: State[I, A],
      f: A => State[I, B]
  ) extends State[I, B]
  private final case class Don[I, A](a: A) extends State[I, A]

  def pure[I, A](a: A): State[I, A] = Don(a)
  def get[I]: State[I, I] = Get(pure)
  def set[I](i: I): State[I, Unit] = Set(i, Don(()))
  def mod[I](f: I => I): State[I, Unit] = Get(i => set(f(i)))
}
