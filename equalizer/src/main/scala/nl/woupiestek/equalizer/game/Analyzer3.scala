package nl.woupiestek.equalizer.game

import Lambda._

object Analyzer3 {

  sealed abstract trait Atom
  final case class Equals(
      pattern: String => Pattern,
      left: Pattern,
      right: Pattern
  ) extends Atom
  final case class Differs(
      pattern: String => Pattern,
      left: Pattern,
      right: Pattern
  ) extends Atom

  def analyze(
      sentence: Sentence,
      heap: Map[String, Pattern],
      counters: Set[Pattern],
      positive: Boolean
  ): Set[Set[Atom]] = {
    def pattern: String => Pattern =
      name =>
        if (positive) Pattern("+" + name, counters.toList)
        else Pattern("-" + name, Nil)

    sentence match {
      case Equation(l, r) =>
        val pat = pattern
        expand(pat, evaluate(l, heap), evaluate(r, heap), Set.empty) match {
          case Some(x) =>
            if (positive) x.map {
              case (a, b) => Set[Atom](Differs(pat, a, b))
            } else
              Set(x.map {
                case (a, b) => Equals(pat, a, b)
              })
          case None =>
            if (positive) Set(Set.empty)
            else Set.empty
        }

      case Implication(a, b) =>
        val c = analyze(a, heap, counters, !positive)
        val d = analyze(b, heap, counters, positive)
        if (positive)
          for {
            e <- c
            f <- d
          } yield e ++ f
        else c ++ d

      case Generalization(varClosure, body) =>
        val p = pattern(varClosure)
        analyze(
          body,
          heap + (varClosure -> p),
          if (positive) counters else counters + p,
          positive
        )
    }
  }

  def expand(
      pattern: String => Pattern,
      left: Lambda,
      right: Lambda,
      eliminate: Set[String]
  ): Option[Set[(Pattern, Pattern)]] = {

    def compare(
        left: Pattern,
        right: Pattern,
        eliminate: Set[String]
    ): Option[Set[(Pattern, Pattern)]] = {
      if (eliminate(left.operator) || eliminate(right.operator)) {
        if (left.operator == right.operator &&
            left.operands.length == right.operands.length) {
          left.operands
            .zip(right.operands)
            .foldLeft(Option.apply(Set.empty[(Pattern, Pattern)])) {
              case (z, (l, r)) =>
                for {
                  f <- expand(pattern, l, r, eliminate)
                  g <- z
                } yield f ++ g
            }
        } else {
          None
        }
      } else {
        Some(Set((left, right)))
      }
    }

    def unfold(
        lambda: Lambda,
        values: List[Pattern]
    ): (Pattern, List[Pattern]) =
      lambda match {
        case Pattern(operator, operands) =>
          (Pattern(operator, operands), values)
        case Closure(term, heap) =>
          term match {
            case Abstraction(varName, body) =>
              val p = pattern(varName)
              unfold(
                evaluate(body, heap + (varName -> p), p :: Nil),
                p :: values
              )
            case other => unfold(evaluate(other, heap, Nil), values)
          }
      }

    val (a, b) = unfold(left, Nil)
    right match {
      case Pattern(operator, operands) =>
        compare(
          a,
          Pattern(operator, operands ++ b.reverse),
          eliminate ++ b.map(_.operator)
        )
      case Closure(term, heap) =>
        val (d, e) = unfold(evaluate(term, heap, b.reverse), Nil)
        compare(
          Pattern(a.operator, a.operands ++ e.reverse),
          d,
          eliminate ++ b.map(_.operator) ++ e.map(_.operator)
        )
    }
  }
}
