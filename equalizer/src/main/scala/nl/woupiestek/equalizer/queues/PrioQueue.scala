package nl.woupiestek.equalizer.queues

import scalaz.Order
import scalaz.Scalaz._

sealed abstract class PrioQueue[P, E]

object PrioQueue {

  private final case class Empty[P, E]() extends PrioQueue[P, E]

  private final case class NonEmpty[P, E](prio: P, node: Node[P, E]) extends PrioQueue[P, E]

  private final case class Node[P, E](elt: E, subs: PrioQueue[P, Node[P, E]])

  def empty[P: Order, E]: PrioQueue[P, E] = Empty[P, E]()

  def singleton[P: Order, E](p: P, e: E): PrioQueue[P, E] = NonEmpty[P, E](p, Node(e, empty))

  def merge[P: Order, E](prioQueue2: PrioQueue[P, Node[P, E]]): PrioQueue[P, E] = prioQueue2 match {
    case Empty() => Empty()
    case NonEmpty(p, Node(Node(e, s0), s1)) =>
      NonEmpty(p, Node(e, s0 ++ merge(s1)))
  }

  implicit class ops[P, E](self: PrioQueue[P, E])(implicit P: Order[P]) {
    def isEmpty: Boolean = self == Empty()

    def insert(p: P, e: E): PrioQueue[P, E] = self ++ singleton(p, e)

    def ++(other: PrioQueue[P, E]): PrioQueue[P, E] =
      (self, other) match {
        case (Empty(), _) => other
        case (_, Empty()) => self
        case (NonEmpty(a, Node(b, c)), NonEmpty(d, Node(e, f))) =>
          if (a <= d) NonEmpty(a, Node(b, c ++ singleton(d, Node(e, f))))
          else NonEmpty(d, Node(e, singleton(a, Node(b, c)) ++ f))
      }

    def headOption: Option[E] = self match {
      case Empty() => None
      case NonEmpty(_, Node(elt, _)) => Some(elt)
    }

    def tailOption: Option[PrioQueue[P, E]] = self match {
      case Empty() => None
      case NonEmpty(_, Node(_, subs)) => Some(merge(subs))
    }
  }


}