package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer2 {

  final case class Prop(left: Lambda, right: Lambda, args: List[Prop])

  def analyzeDeeply(sentence: Sentence): (Prop, List[(String, Int)]) = {
    def helper(
        in: List[(Boolean, Sentence, Map[String, Lambda])],
        out: List[(Sentence, Lambda, Lambda, List[Sentence])],
        cExamples: List[(String, Int)],
        examples: List[(String, Int)],
        offset: Int
    ): (Prop, List[(String, Int)]) = {
      in match {
        case Nil =>
          (out.foldLeft(Map.empty[Sentence, Prop]) {
            case (a, (b, c, d, f)) => a + (b -> Prop(c, d, f.map(a)))
          }(sentence), examples)
        case (a, b, c) :: t =>
          val u =
            if (a) Nil
            else
              cExamples.map {
                case (p, q) => Pattern(p, q, Nil)
              }

          val (d, e, g, k, h) =
            analyze(Pattern(_, _, u), b, c, Nil, Nil, offset)
          helper(
            g.map { case (i, j) => (!a, i, j) } ++ t,
            (b, d, e, g.map(_._1)) :: out,
            (if (a) k else Nil) ++ cExamples,
            (if (a) Nil else k) ++ examples,
            h
          )
      }
    }
    helper((true, sentence, Map.empty[String, Lambda]) :: Nil, Nil, Nil, Nil, 0)
  }

  @tailrec def analyze(
      value: (String, Int) => Lambda,
      sentence: Sentence,
      heap: Map[String, Lambda],
      sentences: List[(Sentence, Map[String, Lambda])],
      values: List[(String, Int)],
      offset: Int
  ): (
      Lambda,
      Lambda,
      List[(Sentence, Map[String, Lambda])],
      List[(String, Int)],
      Int
  ) = sentence match {
    case Equation(l, r) =>
      (eval(l, heap), eval(r, heap), sentences, values, offset)
    case Implication(a, b) =>
      analyze(value, b, heap, (a, heap) :: sentences, values, offset)
    case Generalization(varName, body) =>
      analyze(
        value,
        body,
        heap + (varName -> value(varName, offset)),
        sentences,
        (varName, offset) :: values,
        offset + 1
      )
  }

  sealed abstract class Lambda
  final case class Pattern(
      name: String,
      offset: Int,
      operands: List[Lambda]
  ) extends Lambda
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
        case Some(Name(t, h))       => eval(t, h, stack)
        case Some(Pattern(a, b, c)) => Pattern(a, b, c ++ stack)
        case None                   => Pattern(name, -1, stack)
      }
  }

}
