package nl.woupiestek.andrej

import scala.annotation.tailrec

object Unifier {

  type Term = Tree[String, String]
  type System = List[(Term, Term)]
  type Solution = List[(String, Term)]

  def solve(system: System): Option[Solution] = {

    @tailrec def solveTailRec(system: System, out: Solution): Option[Solution] = {
      system match {
        case Nil => Some(out)
        case head :: tail =>
          def sTail(a: String, b: Term) = tail.map { case (x, y) => (substitute(a, b, x), substitute(a, b, y)) }
          def sOut(a: String, b: Term) = out.map { case (x, y) => (x, substitute(a, b, y)) }
          head match {
            case (a, b) if a == b => solveTailRec(tail, out)
            case (Node(a, b), Node(c, d)) if a == c && b.length == d.length => solveTailRec((b zip d) ++ system, out)
            case (Leaf(a), Node(b, c)) if !occurs(a, c) => solveTailRec(sTail(a, Node(b, c)), sOut(a, Node(b, c)))
            case (Node(b, c), Leaf(a)) if !occurs(a, c) => solveTailRec(sTail(a, Node(b, c)), sOut(a, Node(b, c)))
            case _ => None
          }
      }
    }

    def occurs(leaf: String, terms: List[Tree[String, String]]): Boolean = terms match {
      case Nil => false
      case head :: tail => head match {
        case Leaf(l) => l == leaf || occurs(leaf, tail)
        case Node(_, c) => occurs(leaf, tail ++ c)
      }
    }

    //there is a tail recursive way to traverse trees...
    def substitute(a: String, b: Term, c: Term): Term = c.flatMap(x => if (x == a) b else Leaf(x))

    solveTailRec(system, Nil)
  }
}

sealed trait Tree[+N, +L] {

  def flatMap[M, N0 >: N](f: L => Tree[N0, M]): Tree[N0, M] = {

    @tailrec def traverse(n: N, left: List[Tree[N0, M]], right: List[Tree[N, L]], env: List[(N, List[Tree[N0, M]], List[Tree[N, L]])]): Tree[N0, M] = {
      right match {
        case Leaf(l) :: tail => traverse(n, f(l) :: left, tail, env)
        case Node(m, c) :: tail => traverse(m, Nil, c, (n, left, tail) :: env)
        case Nil =>
          val node: Tree[N0, M] = Node(n, left.reverse)
          env match {
            case Nil => node
            case (m, l, r) :: tail => traverse(m, node :: l, r, tail)
          }
      }
    }

    this match {
      case Leaf(l) => f(l)
      case Node(n, c) => traverse(n, Nil, c, Nil)
    }
  }

  def map[M](f: L => M) = flatMap(l => Leaf(f(l)))

}

case class Leaf[+L](label: L) extends Tree[Nothing, L]

case class Node[+N, +L](label: N, children: List[Tree[N, L]]*) extends Tree[N, L]