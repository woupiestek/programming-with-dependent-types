package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer {

  final case class Value(name: String, offset: Int)

  final case class Sequent(
      left: Lambda,
      right: Lambda,
      ante: Set[Sequent],
      args: Set[Value]
  )

  def judge(
      sequent: Sequent,
      conjunction: List[Sequent],
      disjuntion: List[List[Sequent]],
      offset: Int
  ): Boolean = sequent match {
    case Sequent(left, right, ante, args) =>
      (left, right) match {
        case (Pattern(lOperator, lOperands), Pattern(rOperator, rOperands)) =>
          if (lOperator == rOperator && lOperands.length == rOperands.length) {
            (lOperands.zip(rOperands).map {
              case (l, r) => Sequent(l(Nil), r(Nil), ante, args)
            } ++ conjunction) match {
              case Nil    => true
              case h :: t => judge(h, t, disjuntion, offset)
            }
          } else {
            ??? //still not there
          }
        case _ =>
          val value = Value("$", offset)
          val s = Pattern(value, Nil) :: Nil
          judge(
            Sequent(left(s), right(s), ante, args + value),
            conjunction,
            disjuntion,
            offset + 1
          )
      }
  }

  def analyze(sentence: Sentence): Sequent = {
    @tailrec def helper(
        in: List[(Sentence, Set[Sentence], Set[Value])],
        out: List[(Sentence, Lambda, Lambda, Set[Sentence], Set[Value])],
        offset: Int
    ): Sequent = in match {
      case Nil =>
        out.foldLeft(Map.empty[Sentence, Sequent]) {
          case (o, (s, l, r, as, vs)) => o + (s -> Sequent(l, r, as.map(o), vs))
        }(sentence)
      case (h, antes, values) :: t =>
        h match {
          case Equation(left, right) =>
            val heap = values.groupBy(_.name).map {
              case (k, v) => k -> Pattern(v.maxBy(_.offset), Nil)
            }
            helper(
              t,
              (
                (
                  h,
                  evaluate(left, heap, Nil),
                  evaluate(right, heap, Nil),
                  antes,
                  values
                )
              ) :: out,
              offset
            )
          case Generalization(varName, body) =>
            helper(
              (body, antes, values + Value(varName, offset)) :: in,
              out,
              offset + 1
            )
          case Implication(ante, con) =>
            helper(
              (ante, Set.empty[Sentence], values) :: (con, antes + ante, values) :: in,
              out,
              offset
            )
        }
    }

    helper(((sentence, Set.empty[Sentence], Set.empty[Value])) :: Nil, Nil, 0)
  }

  sealed abstract class Lambda extends (List[Lambda] => Lambda)
  final case class Pattern(operator: Value, operands: List[Lambda])
      extends Lambda {
    def apply(lambdas: List[Lambda]): Lambda =
      Pattern(operator, operands ++ lambdas)
  }
  final case class Closure(term: Term, heap: Map[String, Lambda])
      extends Lambda {
    def apply(lambdas: List[Lambda]): Lambda = evaluate(term, heap, lambdas)
  }

  @tailrec def evaluate(
      term: Term,
      heap: Map[String, Lambda],
      stack: List[Lambda]
  ): Lambda = term match {
    case Application(operator, operand) =>
      evaluate(operator, heap, Closure(operand, heap) :: stack)
    case Abstraction(varName, body) =>
      stack match {
        case Nil    => Closure(term, heap)
        case h :: t => evaluate(body, heap + (varName -> h), t)
      }
    case Let(varName, value, body) =>
      evaluate(body, heap + (varName -> Closure(value, heap)), stack)
    case TermVar(varName) =>
      heap.get(varName) match {
        case Some(Closure(t, h)) => evaluate(t, h, stack)
        case Some(Pattern(x, y)) => Pattern(x, y ++ stack)
        case None                => Pattern(Value(varName, -1), stack)
      }
  }

}
