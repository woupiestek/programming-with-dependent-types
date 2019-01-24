package nl.woupiestek.equalizer.queues

import scalaz._
import scalaz.syntax.order._

import scala.annotation.tailrec
import scala.language.reflectiveCalls

sealed abstract class PrioQueue2[P, E]

object PrioQueue2 {

  final case class Empty[P, E]() extends PrioQueue2[P, E]

  private final case class NonEmpty[P, E](priority: P, node: Node[P, E]) extends PrioQueue2[P, E]

  private final case class Node[P: Order, E](
    head: E,
    front: List[NonEmpty[P, E]],
    back: List[NonEmpty[P, E]]) {

    lazy val tail: PrioQueue2[P, E] = (front, back) match {
      case (Nil, Nil) => Empty()
      case (Nil, h :: t) => h.reverseMergeAll(t)
      case (h :: t, Nil) => h.mergeAll(t)
      case (h0 :: t0, h1 :: t1) => h0.mergeAll(t0).merge(h1.reverseMergeAll(t1))
    }
  }

  def singleton[P: Order, E](p: P, e: E): PrioQueue2[P, E] =
    NonEmpty(p, Node(e, Nil, Nil))

  implicit class ops[P: Order, E](self: PrioQueue2[P, E]) {
    def merge(other: PrioQueue2[P, E]): PrioQueue2[P, E] = (self, other) match {
      case (Empty(), _) => other
      case (_, Empty()) => self
      case (NonEmpty(a, b), NonEmpty(c, d)) =>
        if (a gt c) {
          val Node(e, f, g) = b
          NonEmpty(a, Node(e, f, NonEmpty(c, d) :: g))
        } else {
          val Node(e, f, g) = d
          NonEmpty(c, Node(e, NonEmpty(a, b) :: f, g))
        }
    }


    def mergeAll(others: List[PrioQueue2[P, E]]): PrioQueue2[P, E] =
      reduceAll(self, others, _ merge _)

    def reverseMergeAll(others: List[PrioQueue2[P, E]]): PrioQueue2[P, E] =
      reduceAll(self, others, (x, y) => y merge x)

    def unfold: Option[(E, PrioQueue2[P, E])] = self match {
      case Empty() => None
      case NonEmpty(_, node) => Some((node.head, node.tail))
    }
  }

  implicit def foldable[P: Order]: Foldable[({type Q[E] = PrioQueue2[P, E]})#Q] = {
    type Q[E] = PrioQueue2[P, E]
    new Foldable[Q] {
      override def foldMap[A, B](fa: Q[A])(f: A => B)(implicit F: Monoid[B]): B = {
        def fm(qa: Q[A], b: B): B = fa match {
          case Empty() => b
          case NonEmpty(_, node) => fm(node.tail, F.append(b, f(node.head)))
        }

        fm(fa, F.zero)
      }

      override def foldRight[A, B](fa: Q[A], z: => B)(f: (A, => B) => B): B = {
        def r(qa: Q[A], la: List[A]): List[A] = fa match {
          case Empty() => la
          case NonEmpty(_, node) => r(node.tail, node.head :: la)
        }

        r(fa, Nil).foldLeft(z)((b, a) => f(a, b))
      }
    }
  }

  @tailrec private def reduceAll[P: Order, E](
    self: PrioQueue2[P, E],
    others: List[PrioQueue2[P, E]],
    f: (PrioQueue2[P, E], PrioQueue2[P, E]) => PrioQueue2[P, E]): PrioQueue2[P, E] =
    others match {
      case Nil => self
      case h :: Nil => f(self, h)
      case h0 :: h1 :: t => reduceAll(f(self, f(h0, h1)), t, f)
    }
}
