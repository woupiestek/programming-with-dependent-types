package nl.woupiestek.equalizer.game

sealed abstract class Term
final case class TermVar(name: String) extends Term
final case class Let(varName: String, value: Term, context: Term) extends Term
final case class Application(operator: Term, operand: Term) extends Term
final case class Abstraction(varName: String, body: Term) extends Term

sealed abstract class Sentence
final case class Equation(left: Term, right: Term) extends Sentence
final case class Implication(ante: Sentence, con: Sentence) extends Sentence
final case class Generalization(varName: String, body: Sentence) extends Sentence

sealed abstract class SimpleType
final case class TypeVar(name: String)
final case class Arrow(source: SimpleType, target: SimpleType)

