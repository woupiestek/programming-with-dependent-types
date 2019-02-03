package nl.woupiestek.equalizer.parsing

import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

sealed abstract class DerivativeParser[+I] {
  def foundMatch: Boolean
}

object DerivativeParser {
  final case object Epsilon extends DerivativeParser[Nothing] {
    override val foundMatch = true
  }
  final case object Empty extends DerivativeParser[Nothing] {
    override val foundMatch = false
  }
  private final case class Union[I](
      left: DerivativeParser[I],
      right: Need[DerivativeParser[I]]
  ) extends DerivativeParser[I] {
    override lazy val foundMatch = collapse(left.foundMatch, right)

    @tailrec private def collapse(
        acc: Boolean,
        next: Need[DerivativeParser[I]]
    ): Boolean =
      acc || (next.value match {
        case Union(l, r) => collapse(l.foundMatch, r)
        case other       => other.foundMatch
      })
  }
  private final case class Concat[I](
      readIf: I => Boolean,
      right: Need[DerivativeParser[I]]
  ) extends DerivativeParser[I] {
    override val foundMatch = false
  }

  def readIf[I](f: I => Boolean): DerivativeParser[I] =
    Concat[I](f, Need(Epsilon))

  private def union[I](
      self: DerivativeParser[I],
      other: Need[DerivativeParser[I]]
  ): DerivativeParser[I] =
    if (self.isEmpty) other.value
    else if (other.value.isEmpty) self
    else Union(self, other)

  private def concat[I](
      self: DerivativeParser[I],
      other: Need[DerivativeParser[I]]
  ): DerivativeParser[I] = self match {
    case Epsilon => other.value
    case Empty   => Empty
    case Union(l, r) =>
      union(concat(l, other), r.map(concat(_, other)))
    case Concat(l, r) =>
      Concat(l.asInstanceOf[I => Boolean], r.map(concat(_, other)))
  }

  implicit class ops[I](val self: DerivativeParser[I]) extends AnyVal {
    def isEmpty = self == Empty

    def |(other: => DerivativeParser[I]): DerivativeParser[I] =
      union(self, Need(other))

    def ~(other: => DerivativeParser[I]): DerivativeParser[I] =
      concat(self, Need(other))

    def derivative(c: I): DerivativeParser[I] = self match {
      case Epsilon => Empty
      case Empty   => Empty
      case Union(left, right) =>
        union(left.derivative(c), right.map(_.derivative(c)))
      case Concat(left, right) => if (left(c)) right.value else Empty
    }

    def apply[F[_]: Foldable](input: F[I]): Boolean = 
      input.foldLeft(self)((p, i) => p.derivative(i)).foundMatch
    
  }
}
