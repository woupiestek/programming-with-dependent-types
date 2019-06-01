package nl.woupiestek.equalizer.game

import scala.annotation.tailrec

object Analyzer2 {

  def play(prop: Prop, offset: Int): (List[Prop], Int) = {
    val (props, offset1) =
      break(prop.pattern, prop.left, prop.right, false, offset)
    val (args2, offset2) =
      prop.args.foldLeft[(List[Prop], Int)]((Nil, offset1)) {
        case ((out, o1), p) =>
          val (qs, o2) = break(p.pattern, p.left, p.right, true, o1)
          (out ++ qs.map {
            case (l, r) => Prop(p.pattern, l, r, p.args)
          }, o2)
      }
    //process conjunction
    (props.map { case (l, r) => Prop(prop.pattern, l, r, args2) }, offset2)
  }

  def break(
      pattern: (String, Int) => Pattern,
      left: Lambda,
      right: Lambda,
      example: Boolean,
      offset: Int
  ): (List[(Pattern, Pattern)], Int) = {

    def helper(
        in: List[(Lambda, Lambda)],
        out: List[(Pattern, Pattern)],
        offset: Int
    ): (List[(Pattern, Pattern)], Int) =
      in match {
        case Nil => (out, offset)
        case (l, r) :: t =>
          val (p, q, o) = equate(pattern, l, r, offset)
          if (p.operator.example == example &&
              p.operator == q.operator &&
              p.operands.length == q.operands.length) {
            helper(p.operands.zip(q.operands) ++ t, out, o)
          } else {
            helper(t, (p, q) :: out, o)
          }
      }

    helper((left, right) :: Nil, Nil, offset)
  }

  final case class Prop(
      pattern: (String, Int) => Pattern,
      left: Pattern,
      right: Pattern,
      args: List[Prop]
  )

  def skolemize(sentence: Sentence): (Prop, Int) = {
    def helper(
        in: List[(Boolean, Sentence, Map[String, Pattern])],
        out: List[
          (Sentence, (String, Int) => Pattern, Pattern, Pattern, List[Sentence])
        ],
        offset: Int
    ): (Prop, Int) = in match {
      case Nil =>
        (out.foldLeft(Map.empty[Sentence, Prop]) {
          case (a, (b, e, c, d, f)) => a + (b -> Prop(e, c, d, f.map(a)))
        }(sentence), offset)
      case (a, b, c) :: t =>
        val u = if (a) Nil else c.values.filter(x => !x.operator.example).toList
        val p = (n: String, o: Int) => Pattern(Value(n, o, !a), u)
        val (d, e, g, h) = analyze(p, b, c, Nil, offset)
        helper(
          g.map { case (i, j) => (!a, i, j) } ++ t,
          (b, p, d, e, g.map(_._1)) :: out,
          h
        )
    }
    helper((true, sentence, Map.empty[String, Pattern]) :: Nil, Nil, 0)
  }

  @tailrec def analyze(
      pattern: (String, Int) => Pattern,
      sentence: Sentence,
      heap: Map[String, Pattern],
      sentences: List[(Sentence, Map[String, Pattern])],
      offset: Int
  ): (
      Pattern,
      Pattern,
      List[(Sentence, Map[String, Pattern])],
      Int
  ) = sentence match {
    case Equation(l, r) =>
      val (p, q, o) = equate(pattern, eval(l, heap), eval(r, heap), offset)
      (p, q, sentences, o)
    case Implication(a, b) =>
      analyze(pattern, b, heap, (a, heap) :: sentences, offset)
    case Generalization(varName, body) =>
      analyze(
        pattern,
        body,
        heap + (varName -> pattern(varName, offset)),
        sentences,
        offset + 1
      )
  }

  def equate(
      pattern: (String, Int) => Pattern,
      left: Lambda,
      right: Lambda,
      offset: Int
  ): (Pattern, Pattern, Int) = {
    def unfold(
        lambda: Lambda,
        values: List[Pattern],
        offset: Int
    ): (Pattern, List[Pattern], Int) =
      lambda match {
        case Pattern(operator, operands) =>
          (Pattern(operator, operands), values, offset)
        case Name(term, heap) =>
          val p = pattern("$", offset)
          unfold(eval(term, heap, p :: Nil), p :: values, offset + 1)
      }
    val (a, b, c) = unfold(left, Nil, offset)
    right match {
      case Pattern(operator, operands) =>
        (a, Pattern(operator, operands ++ b.reverse), c)
      case Name(term, heap) =>
        val (d, e, f) = unfold(eval(term, heap, b.reverse), Nil, c)
        (Pattern(a.operator, a.operands ++ e.reverse), d, f)
    }
  }

  final case class Value(name: String, offset: Int, example: Boolean)

  sealed abstract class Lambda
  final case class Pattern(operator: Value, operands: List[Lambda])
      extends Lambda
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
        case None                => Pattern(Value(name, -1, false), stack)
      }
  }

}
