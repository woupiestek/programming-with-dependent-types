package nl.woupiestek.equalizer.l1

import scalaz._

object Lexer {

  sealed abstract class Token
  final case object LParen extends Token
  final case object RParen extends Token
  final case object Comma extends Token
  final case object LAngle extends Token
  final case object RAngle extends Token
  final case class Identifier(name: String) extends Token
  final case object Equals extends Token
  final case object Semicolon extends Token
  final case object Arrow extends Token
  final case object Fixpoint extends Token
  final case object Accent extends Token
  final case class Index(value: Int) extends Token

  final case class State()

  def lex[F[_]: Foldable](chars: F[Char]) = ???

}
