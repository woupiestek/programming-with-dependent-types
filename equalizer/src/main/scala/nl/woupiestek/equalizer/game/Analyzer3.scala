package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer3 {

  sealed abstract trait Atom
  final case class Equals(pattern: Vars[Pattern], left: Pattern, right: Pattern)
      extends Atom
  final case class Differs(
      pattern: Vars[Pattern],
      left: Pattern,
      right: Pattern
  ) extends Atom

  def analyze(
      sentence: Sentence,
      heap: Map[String, Pattern] = Map.empty,
      positive: Boolean = false
  ): Vars[Set[Set[Atom]]] = {
    def pattern: Vars[Pattern] =
      Vars.fresh.map(
        i =>
          if (positive)
            Pattern(
              Existential(i),
              heap.values.filterNot(_.operator.positive).toList
            )
          else Pattern(Universal(i), Nil)
      )

    sentence match {
      case Equation(l, r) =>
        val pat = pattern
        expand(pat, eval(l, heap), eval(r, heap), Set.empty).map {
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
        for {
          c <- analyze(a, heap, !positive)
          d <- analyze(b, heap, positive)
        } yield
          if (positive)
            for {
              e <- c
              f <- d
            } yield e ++ f
          else c ++ d

      case Generalization(varName, body) =>
        pattern.flatMap(
          p => analyze(body, heap + (varName -> p), positive)
        )
    }
  }

  def expand(
      pattern: Vars[Pattern],
      left: Lambda,
      right: Lambda,
      eliminate: Set[Var]
  ): Vars[Option[Set[(Pattern, Pattern)]]] = {

    def compare(
        left: Pattern,
        right: Pattern,
        eliminate: Set[Var]
    ): Vars[Option[Set[(Pattern, Pattern)]]] = {
      if (eliminate(left.operator) || eliminate(right.operator)) {
        if (left.operator == right.operator &&
            left.operands.length == right.operands.length) {
          left.operands
            .zip(right.operands)
            .foldLeft(Vars.pure(Option.apply(Set.empty[(Pattern, Pattern)]))) {
              case (z, (l, r)) =>
                for {
                  d <- expand(pattern, l, r, eliminate)
                  e <- z
                } yield
                  for {
                    f <- d
                    g <- e
                  } yield f ++ g
            }
        } else {
          Vars.pure(None)
        }
      } else {
        Vars.pure(Some(Set((left, right))))
      }
    }

    def unfold(
        lambda: Lambda,
        values: List[Pattern]
    ): Vars[(Pattern, List[Pattern])] =
      lambda match {
        case Pattern(operator, operands) =>
          Vars.pure((Pattern(operator, operands), values))
        case Name(term, heap) =>
          pattern.flatMap(
            p => unfold(eval(term, heap, p :: Nil), p :: values)
          )
      }

    unfold(left, Nil).flatMap {
      case (a, b) =>
        right match {
          case Pattern(operator, operands) =>
            compare(
              a,
              Pattern(operator, operands ++ b.reverse),
              eliminate ++ b.map(_.operator)
            )
          case Name(term, heap) =>
            unfold(eval(term, heap, b.reverse), Nil).flatMap {
              case (d, e) =>
                compare(
                  Pattern(a.operator, a.operands ++ e.reverse),
                  d,
                  eliminate ++ b.map(_.operator) ++ e.map(_.operator)
                )
            }
        }
    }
  }

  sealed abstract class Var {
    def positive = !isInstanceOf[Universal]
  }
  final case class Universal(offset: Int) extends Var
  final case class Existential(offset: Int) extends Var
  final case class Unbound(name: String) extends Var

  sealed abstract class Lambda
  final case class Pattern(operator: Var, operands: List[Lambda]) extends Lambda
  final case class Name(term: Term, heap: Map[String, Lambda]) extends Lambda

  @tailrec def eval(
      term: Term,
      heap: Map[String, Lambda] = Map.empty,
      stack: List[Lambda] = Nil
  ): Lambda = term match {
    case Abstraction(varName, body) =>
      stack match {
        case Nil    => Name(term, heap)
        case h :: t => eval(body, heap + (varName -> h), t)
      }
    case Application(operator, operand) =>
      eval(operator, heap, Name(operand, heap) :: stack)
    case Let(varName, value, context) =>
      eval(context, heap + (varName -> Name(value, heap)), stack)
    case TermVar(name) =>
      heap.get(name) match {
        case Some(Name(t, h))    => eval(t, h, stack)
        case Some(Pattern(a, b)) => Pattern(a, b ++ stack)
        case None                => Pattern(Unbound(name), stack)
      }
  }
}
