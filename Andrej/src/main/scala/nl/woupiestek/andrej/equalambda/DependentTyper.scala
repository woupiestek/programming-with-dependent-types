package nl.woupiestek.andrej.equalambda

sealed trait DTerm

case class Var(name: String) extends DTerm

case class App(operator: DTerm, operand: DTerm) extends DTerm

case class Abs(name: String, body: DTerm) extends DTerm

case class Uni(left: DTerm, right: DTerm, term: DTerm) extends DTerm

case class Che(left: DTerm, right: DTerm, term: DTerm) extends DTerm

sealed trait Dom

case class Pair(name: String, dom: Cod) extends Dom

case class Equa(left: DTerm, right: DTerm) extends Dom

sealed trait Cod

case class Par(index: Int) extends Cod

case class Par2(index: Int, arg: DTerm) extends Cod

case class Arrow(dom: Dom, cod: Cod) extends Cod


object DependentTyper {

  sealed trait Q

  case class QTy(left: Cod, right: Cod, context: List[Dom]) extends Q

  case class QTe(left: DTerm, right: DTerm, context: List[Dom]) extends Q

  def rules(term: DTerm): (Cod, List[Dom], Int) => List[Q] = term match {
    case Var(name) => vari(name)
    case App(x, y) => appl(x, y)
    case Abs(n, b) => abst(n, b)
    case Uni(l, r, t) => unify(l, r, t)
    case Che(l, r, t) => check(l, r, t)
  }

  private def check(l: DTerm, r: DTerm, t: DTerm)(cod: Cod, context: List[Dom], i: Int): List[Q] =
    QTe(l, r, context) :: rules(l)(Par(i), context, i + 1) ++ rules(r)(Par(i), context, i + 1) ++
      rules(t)(Arrow(Equa(l, r), cod), context, i + 1)

  private def unify(l: DTerm, r: DTerm, t: DTerm)(cod: Cod, context: List[Dom], i: Int): List[Q] =
    QTy(Arrow(Equa(l, r), cod), Par(i), context) :: rules(l)(Par(i), context, i + 2) ++
      rules(r)(Par(i), context, i + 2) ++ rules(t)(Par(i + 1), Equa(l, r) :: context, i + 2)

  private def abst(n: String, b: DTerm)(cod: Cod, context: List[Dom], i: Int): List[Q] =
    QTy(Arrow(Pair(n, Par(i)), Par2(i + 1, Var(n))), cod, context) ::
      rules(b)(Par2(i + 1, Var(n)), Pair(n, Par(i)) :: context, i + 2)

  private def appl(x: DTerm, y: DTerm)(cod: Cod, context: List[Dom], i: Int): List[Q] =
    QTy(Par2(i + 1, y), cod, context) :: rules(x)(Arrow(Pair(s"$i", Par(i)), Par2(i + 1, Var(s"$i"))), context, i + 2) ++
      rules(y)(Par(i), context, i + 2)

  private def vari(name: String)(cod: Cod, context: List[Dom], i: Int): List[Q] = context.collectFirst {
    case Pair(n, d) if name == n => QTy(cod, d, context)
  }.toList

  case class R(assumption: T, target: S)

  case class S(target: Cod, context: List[Dom])

  case class T(target: DTerm, context: List[Dom])

  def solve1(assignment: Map[Int, S],
    assignment2: Map[Int, R],
    assumptions: List[(T, T)],
    equations: List[(S, S)]): Map[Int, S] =
    equations match {
      case Nil => assignment
      case (S(Par(i), _), x) :: t =>
        if (assignment.contains(i)) solve1(assignment, assignment2, assumptions, (assignment(i), x) :: t)
        else solve1(assignment + (i -> x), assignment2, assumptions, t)
      case (x, S(Par(i), _)) :: t =>
        if (assignment.contains(i)) solve1(assignment, assignment2, assumptions, (assignment(i), x) :: t)
        else solve1(assignment + (i -> x), assignment2, assumptions, t)
      case (S(Par2(i, m), c), x) :: t =>
        if (assignment2.contains(i)) {
          val R(t, y) = assignment2(i)
          solve1(assignment, assignment2, (T(m, c), t) :: assumptions, (x, y) :: equations)
        } else solve1(assignment, assignment2 + (i -> R(T(m, c), x)), assumptions, t)
      case (x, S(Par2(i, m), c)) :: t =>
        if (assignment2.contains(i)) {
          val R(t, y) = assignment2(i)
          solve1(assignment, assignment2, (T(m, c), t) :: assumptions, (x, y) :: equations) //assumptions out of context
        } else solve1(assignment, assignment2 + (i -> R(T(m, c), x)), assumptions, t)
      case (S(Arrow(a, b), c), S(Arrow(d, e), f)) :: t => //here we run into a new problem: dependencies between subterms
        solve1(assignment, assignment2, assumptions, (S(b, a :: c), S(e, d :: f)) :: t)
      case _ => ??? //failure
    }

  case class U(cod: Cod, mask: Map[String, Int])

  case class V(pattern: DTerm, mask: Map[String, Int])

  case class W(v: V, u: U)

  case class X(l: U, r: U, us: List[(V, V)])//these unification still have a role to play

  //introduce equations of terms.
  def solve2(
    assignment1: Map[Int, U],
    assignment2: Map[Int, W],
    equations: List[X],
    index: Int): Map[Int, U] = equations match {
    case Nil => assignment1
    case X(U(Par(a), _), b, d) :: c =>
      if (assignment1.contains(a)) solve2(assignment1, assignment2, X(assignment1(a), b, d) :: c, index)//incidence check!
      else solve2(assignment1 + (a -> b), assignment2, c, index)
    case X(b, U(Par(a), _), d) :: c =>
      if (assignment1.contains(a)) solve2(assignment1, assignment2, X(assignment1(a), b, d) :: c, index)//incidence check!
      else solve2(assignment1 + (a -> b), assignment2, c, index)
    case X(U(Par2(a, b), c), d, h) :: e =>
      if (assignment2.contains(a)) {
        val W(f, g) = assignment2(a)//incidence check!
        solve2(assignment1, assignment2, X(d, g, (f, V(b, c)) :: h) :: equations, index)
      } else solve2(assignment1, assignment2 + (a -> W(V(b, c), d)), e, index)
    case X(d, U(Par2(a, b), c), h) :: e =>
      if (assignment2.contains(a)) {
        val W(f, g) = assignment2(a)//incidence check!
        solve2(assignment1, assignment2, X(d, g, (f, V(b, c)) :: h) :: equations, index)
      } else solve2(assignment1, assignment2 + (a -> W(V(b, c), d)), e, index)
    case X(U(Arrow(a, b), c), U(Arrow(d, e), f), l) :: g => (a, d) match {
      case (Pair(h, i), Pair(j, k)) => solve2(assignment1, assignment2,
        X(U(i, c), U(k, f), l) :: X(U(b, c + (h -> index)), U(e, f + (j -> index)), l) :: g, index + 1)
      case (Equa(h, i), Equa(j, k)) => //these should play a role in solving other equations
        solve2(assignment1, assignment2,
          X(U(b, c), U(e, f), (V(h, c), V(j, f)) :: (V(i, c), V(k, f)) :: l) :: equations, index)
      case _ => ??? //fail
    }
    case _ => ??? //fail
  }


}
