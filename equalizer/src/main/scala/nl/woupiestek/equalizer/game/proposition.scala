package nl.woupiestek.equalizer.game
import nl.woupiestek.equalizer.game.Variable.Generated
import nl.woupiestek.equalizer.game.Variable.Named
import nl.woupiestek.equalizer.game.PTerm.Where
import nl.woupiestek.equalizer.game.PNode.Compare
import scalaz.Coproduct
import nl.woupiestek.equalizer.game.PEdge.Connect
import nl.woupiestek.equalizer.game.PEdge.Quantify

sealed trait PNode

object PNode {
  case class Compare(left: PTerm, right: PTerm, equal: Boolean)
      extends PNode
}

sealed trait PEdge

object PEdge {
  type Identifier = Variable
  case class Connect(index: Int, conjoin: Boolean) extends PEdge
  case class Quantify(identifier: Identifier, forall: Boolean)
      extends PEdge
}

sealed trait Variable
object Variable {
  case class Named(name: String) extends Variable
  case class Generated(origin: List[PEdge]) extends Variable
}
sealed trait PTerm

object PTerm {
  case class Application(
      operator: Variable,
      operands: List[PTerm]
  ) extends PTerm
  case class Abstraction(
      arg: String,
      body: PTerm
  ) extends PTerm
  case class Where(term: PTerm, heap: Map[Variable, PTerm])
      extends PTerm

  def reduce(
      term: PTerm,
      heap: Map[Variable, PTerm],
      stack: List[PTerm]
  ): PTerm =
    term match {
      case Abstraction(arg, body) =>
        stack match {
          case Nil => Where(Abstraction(arg, body), heap)
          case head :: tl =>
            reduce(body, heap + (Named(arg) -> head), tl)
        }
      case Application(operator, operands) =>
        val newStack = operands.map(Where(_, heap)) ++ stack
        if (heap.contains(operator))
          reduce(heap(operator), Map.empty, newStack)
        else Application(operator, newStack)
      case Where(term, heap2) =>
        reduce(
          term,
          heap ++ heap2.mapValues(Where(_, heap)),
          stack
        )
    }

  def isClosure(pTerm: PTerm): Boolean =
    pTerm.isInstanceOf[Application] ||
      (pTerm.isInstanceOf[Where] && isClosure(
        pTerm.asInstanceOf[Where].term
      ))
}

case class Proposition(nodes: Map[List[PEdge], PNode])

object Proposition {
  def compare(
      edges: List[PEdge],
      left: PTerm,
      right: PTerm,
      equal: Boolean
  ): (List[PEdge], PNode) =
    if (PTerm.isClosure(left) || PTerm.isClosure(right)) {
      val fresh = Generated(edges)
      val fresh2 = PTerm.Application(fresh, Nil)
      compare(
        PEdge.Quantify(fresh, equal) :: edges,
        PTerm.reduce(left, Map.empty, fresh2 :: Nil),
        PTerm.reduce(right, Map.empty, fresh2 :: Nil),
        equal
      )
    } else if (left.isInstanceOf[PTerm.Where] || right
                 .isInstanceOf[PTerm.Where])
      compare(
        edges,
        PTerm.reduce(left, Map.empty, Nil),
        PTerm.reduce(right, Map.empty, Nil),
        equal
      )
    else (edges, Compare(left, right, equal))

  def skolemize(
      edges: List[PEdge],
      left: PTerm,
      right: PTerm,
      equal: Boolean
  ): (List[PEdge], PNode) = {
    def _edges(
        in: List[PEdge],
        heap: Map[Variable, List[Variable]],
        out: List[PEdge],
        globals: List[String]
    ): (List[PEdge], PNode) = in match {
      case Nil =>
        val _heap = heap.map {
          case (operator, operands) =>
            operator -> PTerm.Application(
              operator,
              operands.map(
                name => PTerm.Application(name, Nil)
              )
            )
        }
        (
          out.reverse ++ globals.reverse.map(
            name => PEdge.Quantify(Named(name), false)
          ),
          Compare(
            PTerm.reduce(left, _heap, Nil),
            PTerm.reduce(right, _heap, Nil),
            equal
          )
        )
      case head :: tl =>
        head match {
          case _: Connect =>
            _edges(tl, heap, head :: out, globals)
          case Quantify(identifier, forall) if forall =>
            val _heap: Map[Variable, List[Variable]] =
              heap.map {
                case (operator, operands) =>
                  operator -> (identifier :: operands)
              }
            _edges(tl, _heap, head :: out, globals)
          case Quantify(identifier, forall) if !forall =>
            _edges(
              tl,
              heap + (identifier -> Nil),
              head :: out,
              globals
            )
        }
    }
    _edges(edges, Map.empty, Nil, Nil)
  }
}
