package nl.woupiestek.andrej.equalambda

object Paramodulation2 {

  sealed trait Term

  case class Pattern(name: String, operands: List[Term])
      extends Term

  case class Apply(operator: Term, operands: List[Term])
      extends Term

  case class Lambda(input: String, body: Term) extends Term

  case class Check(left: Term, right: Term, term: Term)
      extends Term

  case class Unify(left: Term, right: Term, term: Term)
      extends Term

  trait Goal

  case class Match(
      lVar: String,
      lArgs: List[Term],
      rVar: String,
      rArgs: List[Term]
  ) extends Goal

  case object False extends Goal

  case class Clause(
      from: List[Clause],
      eqs: Set[(String, String)],
      to: Goal
  )

  def variable(name: String): Term = Pattern(name, Nil)

  def clauses(
      from: List[Clause],
      eqs: Set[(String, String)],
      left: Term,
      right: Term
  ): List[Clause] = {
    def negate: List[Clause] = List(Clause(from, eqs, False))

    left match {
      case Pattern(a, b) =>
        right match {
          case Pattern(c, d) =>
            List(Clause(from, eqs, Match(a, b, c, d)))
          case _ => clauses(from, eqs, right, left)
        }
      case Apply(a, b) =>
        b match {
          case Nil => clauses(from, eqs, a, right)
          case bh :: bt =>
            a match {
              case Pattern(c, d) =>
                clauses(from, eqs, Pattern(c, d ++ b), right)
              case Apply(c, d) =>
                clauses(from, eqs, Apply(c, d ++ b), right)
              case Lambda(c, d) =>
                clauses(
                  clauses(Nil, Set.empty, bh, variable(c)) ++ from,
                  eqs,
                  Apply(d, bt),
                  right
                )
              case _ => negate
            }
        }
      case Lambda(a, b) =>
        right match {
          case Lambda(c, d) =>
            clauses(from, eqs + ((a, c)), b, d)
          case other =>
            clauses(
              from,
              eqs,
              b,
              Apply(other, variable(a) :: Nil)
            )
        }
      case Check(a, b, c) =>
        right match {
          case Pattern(_, _) | Apply(_, _) =>
            clauses(from, eqs, right, left)
          case Check(d, e, f) =>
            clauses(from, eqs, a, d) ++ clauses(from, eqs, b, e) ++ clauses(
              from,
              eqs,
              c,
              f
            )
          case _ => negate
        }
      case Unify(a, b, c) =>
        right match {
          case Pattern(_, _) | Apply(_, _) =>
            clauses(from, eqs, right, left)
          case Unify(d, e, f) =>
            clauses(from, eqs, a, d) ++ clauses(from, eqs, b, e) ++ clauses(
              from,
              eqs,
              c,
              f
            )
          case _ => negate
        }
    }
  }

  def recombine(
      c: List[Clause],
      d: Set[(String, String)],
      a: Clause
  ) = Clause(c ++ a.from, d ++ a.eqs, a.to)

  def recombine(
      c: List[Clause],
      d: Set[(String, String)],
      a: (String, String)
  ): Clause = Clause(c, d, Match(a._1, Nil, a._2, Nil))

  def recombine(
      a: List[Clause],
      b: Set[(String, String)],
      c0: List[Term],
      c1: String,
      c2: List[Term],
      d0: List[Term],
      d1: String,
      d2: List[Term]
  ): Clause = {
    c0.length - d0.length match {
      case e if e < 0 =>
        val (d3, d4) = d0.splitAt(c0.length)
        Clause(a ++ c0.zip(d3).flatMap {
          case (c, d) => clauses(a, b, c, d)
        }, b, Match(c1, c2, d1, d2 ++ d4))
      case 0 =>
        Clause(a ++ c0.zip(d0).flatMap {
          case (c, d) => clauses(a, b, c, d)
        }, b, Match(c1, c2, d1, d2))
        val (c3, c4) = c0.splitAt(d0.length)
        Clause(a ++ c3.zip(d0).flatMap {
          case (c, d) => clauses(a, b, c, d)
        }, b, Match(c1, c2 ++ c4, d1, d2))
    }
  }

  def resolve: Clause => List[List[Clause]] = {
    case Clause(a, b, Match(c, d, e, f)) =>
      a map {
        case Clause(g, h, Match(i, j, k, l)) if c == i =>
          recombine(g, h, d, e, f, j, k, l) :: a.map(
            recombine(g, h, _)
          ) ++ b.map(recombine(g, h, _))
        case Clause(g, h, Match(i, j, k, l)) if c == k =>
          recombine(g, h, d, e, f, l, i, j) :: a.map(
            recombine(g, h, _)
          ) ++ b.map(recombine(g, h, _))
        case Clause(g, h, Match(i, j, k, l)) if e == i =>
          recombine(g, h, f, c, d, j, k, l) :: a.map(
            recombine(g, h, _)
          ) ++ b.map(recombine(g, h, _))
        case Clause(g, h, Match(i, j, k, l)) if e == k =>
          recombine(g, h, f, c, d, l, i, j) :: a.map(
            recombine(g, h, _)
          ) ++ b.map(recombine(g, h, _))
        case Clause(g, h, False) =>
          a.map(recombine(g, h, _)) ++ b.map(recombine(g, h, _))
      }
    case Clause(a, b, False) =>
      a map {
        case Clause(c, d, False) =>
          a.map(recombine(c, d, _)) ++ b.map(recombine(c, d, _))
        case _ => Nil
      }
    case _ => Nil
  }

}
