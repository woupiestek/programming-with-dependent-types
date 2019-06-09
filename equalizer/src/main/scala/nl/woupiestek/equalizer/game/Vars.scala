package nl.woupiestek.equalizer.game
import Vars._

sealed abstract class Vars[Y] {

  def flatMap[Z](f: Y => Vars[Z]): Vars[Z] =
    this match {
      case Done(x)          => f(x)
      case cont: Cont[b, c] => Cont(cont.y, cont.f(_: b).flatMap(f))
      case incr @ Incr(_)   => Cont(incr, f)
    }
  def map[Z](f: Y => Z) = flatMap(y => Done(f(y)))

  @annotation.tailrec
  def run(w: Int): Y = this match {
    case Done(result) => result
    case Incr(step)   => step(w).run(w + 1)
    case Cont(y, f) =>
      y match {
        case Done(z)    => f(z).run(w)
        case Incr(z)    => z(w).flatMap(f).run(w + 1)
        case Cont(z, g) => z.flatMap(g(_).flatMap(f)).run(w)
      }
  }
}

object Vars {
  private final case class Done[Y](result: Y) extends Vars[Y]
  private final case class Incr[Y](step: Int => Vars[Y]) extends Vars[Y]
  private final case class Cont[Y, Z](
      y: Vars[Y],
      f: Y => Vars[Z]
  ) extends Vars[Z]

  def tailRec[X, Z](x: X)(f: X => Vars[Either[X, Z]]): Vars[Z] =
    f(x).flatMap {
      case Left(y)  => tailRec(y)(f)
      case Right(y) => Done(y)
    }

  def pure[Y](y: Y): Vars[Y] = Done(y)
  def fresh: Vars[Int] = Incr(pure)

}
