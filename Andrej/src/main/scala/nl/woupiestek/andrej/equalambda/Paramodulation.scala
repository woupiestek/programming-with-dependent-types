package nl.woupiestek.andrej.equalambda

object Paramodulation {

  /* Assumptions
   * - all terms are normalizable
   * - variable capture is no problem, because bound variables have different names
   */
  //we'll assume that variable capture is not a problem because all binding have unique names

  //also note: no check and unify... because they can cause problems during evaluation
  //without evaluation this gets better.

  sealed trait Term

  case class Pattern(name: String, operands: List[Term])
      extends Term

  def variable(name: String): Term = Pattern(name, Nil)

  case class Apply(operator: Term, operands: List[Term])
      extends Term

  case class Where(term: Term, subs: Map[String, Term])
      extends Term

  case class Lambda(input: String, body: Term) extends Term

  case class Reflect(left: Term, right: Term) extends Term

  case class Unify(left: Term, right: Term, term: Term)
      extends Term

  sealed trait Proposition

  case class Equation(
      x: Term,
      xSubs: Map[String, Term],
      y: Term
  ) extends Proposition

  case class Match(
      lHead: String,
      lArgs: List[Term],
      rHead: String,
      rArgs: List[Term]
  ) extends Proposition

  case object False extends Proposition

  def reduce(
      left: Term,
      lSubs: Map[String, Term],
      right: Term
  ): List[Proposition] = {
    def turn: List[Proposition] =
      List(Equation(right, Map.empty, Where(left, lSubs)))

    def deny: List[Proposition] = List(False)

    left match {
      case Pattern(a, b) =>
        lSubs.get(a) match {
          case Some(c) =>
            List(Equation(Apply(c, b), lSubs, right))
          case None =>
            right match {
              case Pattern(c, d)
                  if a == c && b.length == d.length => //I doubt this!
                b.zip(d).map {
                  case (bi, di) => Equation(bi, lSubs, di)
                }
              case Pattern(c, d) =>
                List(Match(a, b.map(Where(_, lSubs)), c, d))
              case Apply(_, _) | Where(_, _) => turn
              case Reflect(_, _) | Unify(_, _, _) =>
                List(Equation(left, lSubs, right))
            }
        }
      case Apply(a, b) =>
        b match {
          case Nil => List(Equation(a, lSubs, right))
          case bh :: bt =>
            a match {
              case Pattern(c, d) =>
                List(Equation(Pattern(c, d ++ b), lSubs, right))
              case Apply(c, d) =>
                List(Equation(Apply(c, d ++ b), lSubs, right))
              case Lambda(c, d) =>
                List(
                  Equation(
                    Apply(d, bt),
                    lSubs + (c -> bh),
                    right
                  )
                )
              case Where(c, d) =>
                List(Equation(Apply(c, b), lSubs ++ d, right))
              case _ => deny
            }
        }
      case Where(a, b) => List(Equation(a, lSubs ++ b, right))
      case Lambda(a, b) =>
        right match {
          case Lambda(c, d) =>
            List(Equation(b, lSubs + (a -> variable(c)), d))
          case other =>
            List(
              Equation(
                b,
                lSubs,
                Apply(other, variable(a) :: Nil)
              )
            )
        }
      case Reflect(a, b) =>
        right match {
          case Reflect(d, e) =>
            List(Equation(a, lSubs, d), Equation(b, lSubs, e))
          case Unify(_, _, _) => deny
          case _              => turn
        }
      case Unify(a, b, c) =>
        right match {
          case Unify(d, e, f) =>
            List(
              Equation(a, lSubs, d),
              Equation(b, lSubs, e),
              Equation(c, lSubs, f)
            )
          case Reflect(_, _) => deny
          case _             => turn
        }
    }
  }

  def fullReduce(
      props: List[Proposition]
  ): List[Proposition] = {
    def helper(
        in: List[Proposition],
        out: List[Proposition]
    ): List[Proposition] = in match {
      case Nil => out
      case h :: t =>
        h match {
          case Equation(a, b, c) =>
            helper(reduce(a, b, c) ++ t, out)
          case Match(_, _, _, _) => helper(t, h :: out)
          case False             => List(False)
        }
    }

    helper(props, Nil)
  }

  def derive(
      to: Proposition,
      from: List[Proposition]
  ): Boolean = {

    def helper(
        to: Match,
        reducedFrom: List[Proposition]
    ): List[(Match, List[Proposition])] = {
      def recombine(
          a: List[Term],
          b: String,
          c: List[Term],
          d: List[Term],
          e: String,
          f: List[Term]
      ): (Match, List[Proposition]) = {
        a.length - d.length match {
          case x if x > 0 =>
            val (a0, a1) = a.splitAt(d.length)
            val eqs = fullReduce(a0.zip(d).map {
              case (ai, di) => Equation(ai, Map.empty, di)
            })
            (Match(b, c, e, f ++ a1), eqs ++ reducedFrom)
          case 0 =>
            val eqs = fullReduce(a.zip(d).map {
              case (ai, di) => Equation(ai, Map.empty, di)
            })
            (Match(b, c, e, f), eqs ++ reducedFrom)
          case x if x < 0 =>
            val (d0, d1) = d.splitAt(a.length)
            val eqs = fullReduce(a.zip(d0).map {
              case (ai, di) => Equation(ai, Map.empty, di)
            })
            (Match(b, c ++ d1, e, f), eqs ++ reducedFrom)
        }
      }

      val Match(a, b, c, d) = to
      val set1 = reducedFrom.collect {
        case Match(e, f, g, h) if a == e =>
          recombine(b, c, d, f, g, h)
        case Match(e, f, g, h) if a == g =>
          recombine(b, c, d, h, e, f)
        case Match(e, f, g, h) if c == e =>
          recombine(d, a, b, f, g, h)
        case Match(e, f, g, h) if c == g =>
          recombine(d, a, b, h, e, f)
      }
      //what about a==c?
      //that would give a conjunction...
      set1
    }

    def helper2(
        options: List[(Match, List[Proposition])]
    ): Boolean = options match {
      case Nil                              => false
      case (_, f) :: t if f.contains(False) => helper2(t)
      case (t, f) :: t2                     => helper2(helper(t, f) ++ t2)
    }

    val reducedFrom = fullReduce(from)
    to match {
      case Equation(a, b, c) =>
        reduce(a, b, c).forall {
          case Match(e, f, g, h) =>
            helper2(helper(Match(e, f, g, h), reducedFrom))
          case _ => false
        }
      case False => reducedFrom.contains(False)
      case Match(a, b, c, d) =>
        helper2(helper(Match(a, b, c, d), reducedFrom))
    }
  }
  //time to do some tests...
}
