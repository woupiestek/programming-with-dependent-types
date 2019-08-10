package nl.woupiestek.equalizer.parsing

import scalaz._
import Scalaz._
import Arithmetic._

class Arithmetic[P[_]: Applicative](
    effects: Effects[P, Token]
) {

  private val operations
      : Map[Token, (Double, Double) => Double] = Map(
    Arithmetic.Divide -> (_ / _),
    Arithmetic.Plus -> (_ + _),
    Minus -> (_ - _),
    Times -> (_ * _)
  )

  def expression(rbp: Int): P[Double] = {

    lazy val tail: P[Double => Double] =
      effects.read(
        key =>
          operations
            .get(key)
            .fold(effects.reject[Double => Double])(
              op =>
                if (key.lbp > rbp)
                  Apply[P]
                    .apply2(expression(key.lbp), tail)(
                      (
                          right: Double,
                          next: Double => Double
                      ) =>
                        (left: Double) =>
                          next(op(left, right))
                    )
                else effects.reject[Double => Double]
            )
      )

    effects.read {
      case Constant(value) => tail.map(_(value)) //fail...
      case LeftParen =>
        expression(0) <*> effects.read {
          case RightParen => tail
          case _          => effects.error("Unmatched '('")
        }
      case Minus => expression(rbp).map(-_)
      case _     => effects.error("Illegal start of expression")
    }
  }
}

object Arithmetic {

  sealed abstract class Token(val lbp: Int)
  final case class Constant(value: Double) extends Token(0)
  final case object Plus extends Token(10)
  final case object Minus extends Token(10)
  final case object Times extends Token(20)
  final case object Divide extends Token(20)
  final case object LeftParen extends Token(0)
  final case object RightParen extends Token(0)

  trait Effects[P[_], T] {
    def read[A](next: T => P[A]): P[A]
    def reject[A]: P[A]
    def error[A](message: => String): P[A]
  }
}
