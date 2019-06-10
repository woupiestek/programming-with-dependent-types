package nl.woupiestek.equalizer.game

sealed abstract class Term
final case class TermVar(name: String) extends Term
final case class Let(varName: String, value: Term, context: Term) extends Term
final case class Application(operator: Term, operand: Term) extends Term
final case class Abstraction(varName: String, body: Term) extends Term

object Term {
  def free(term: Term): Set[String] = {
    @annotation.tailrec
    def helper(in: List[(Term, Set[String])], out: Set[String]): Set[String] =
      in match {
        case Nil => out
        case (h0, h1) :: t =>
          h0 match {
            case TermVar(name) => helper(t, if (h1(name)) out else out + name)
            case Abstraction(varName, body) =>
              helper((body, h1 + varName) :: t, out)
            case Application(operator, operand) =>
              helper((operator, h1) :: (operand, h1) :: t, out)
            case Let(varName, value, context) =>
              helper((value, h1) :: (context, h1 + varName) :: t, out)
          }
      }
    helper((term, Set.empty[String]) :: Nil, Set.empty[String])
  }
}

sealed abstract class Sentence
final case class Equation(left: Term, right: Term) extends Sentence
final case class Implication(ante: Sentence, con: Sentence) extends Sentence
final case class Generalization(varName: String, body: Sentence)
    extends Sentence

object Sentence {
  def free(sentence: Sentence): Set[String] = {
    @annotation.tailrec
    def helper(
        in: List[(Sentence, Set[String])],
        out: Set[String]
    ): Set[String] =
      in match {
        case Nil => out
        case (h0, h1) :: t =>
          h0 match {
            case Equation(left, right) =>
              helper(t, (Term.free(left) ++ Term.free(right) -- h1) ++ out)
            case Implication(ante, con) =>
              helper((ante, h1) :: (con, h1) :: t, out)
            case Generalization(varName, body) =>
              helper((body, h1 + varName) :: t, out)
          }
      }
    helper((sentence, Set.empty[String]) :: Nil, Set.empty[String])
  }
}

sealed abstract class SimpleType
final case class TypeVar(name: String)
final case class Arrow(source: SimpleType, target: SimpleType)
