package nl.woupiestek.whynot

import scala.annotation.tailrec

/*
 * radix/bucket based priority queue
 *
 * What I have in mind simply has 31 levels with 2 buckets
 * (0 and 1) each. Maybe it would be easier to have 8 levels
 * of 16 buckets, or even fewer levels with even more buckets
 * At least we can safe space, by truncating the empty trees,
 * and maybe we can safe time as well...
 *
 * Think of that truncated binary tree, but reorganize it:
 * imagine the traversal, rewire to make the least node
 * the starting point instead of the root.
 *
 * That helps?
 *
 * Using recursive bucket search to find the least element
 * supposedly looks somewhat like this. Split the elements up
 * according to the highest bit of priority. Take the highest
 * priority bucket that is not empty and repeat the process for
 * the next highest digit, than the next highest digit after
 * that etc. After going through all digits, this should lead
 * to the least element, in a time that is bounded by an
 * constant.
 *
 * The PQ datastructure could reflect that: each level contain
 * a simple queue for low prio elements, and either a priority
 * queue for higher priority elements, or the highest priority
 * element itself.
 *
 */

case class PQ[A](
    higherPrio: Either[PQ[A], (Int, A)],
    lowerPrio: List[(Int, A)],
    threshold: Int
) {
  def enqueue(prio: Int, value: A): PQ[A] = {
    if (prio >= threshold) {
      copy(lowerPrio = (prio, value) :: lowerPrio)
    } else {
      higherPrio match {
        case Left(q) =>
          copy(higherPrio = Left(q.enqueue(prio, value)))
        case Right((p, v)) =>
          val (higher, lower) =
            if (prio < p) ((prio, value), (p, v))
            else ((p, v), (prio, value))
          PQ(Right(higher), lower :: Nil, p + prio / 2) // this is good!
      }
    }
  }
  def head: A = {
    higherPrio match {
      case Left(value)  => value.head
      case Right(value) => value._2
    }
  }
  def tail: Option[PQ[A]] = {
    def first = higherPrio match {
      case Left(value) =>
        value.tail.map(q => copy(higherPrio = Left(q)))
      case Right(_) => None
    }
    def second = lowerPrio.reverse match {
      case Nil => None
      case h :: t =>
        Some(t.foldLeft(PQ(Right(h), Nil, h._1)) {
          case (q, (p, a)) => q.enqueue(p, a)
        })
    }
    first orElse second
  }
}

object PQ {
  def apply[A](prio: Int, value: A): PQ[A] =
    PQ(Right((prio, value)), Nil, prio)
}

/*
 * Above I use an average instead of a predetermined threshold,
 * which simplifies calculation without completely relying on
 * comparisons.
 *
 * Now rewire the tree to make access to the high priority end
 * less costly
 */
case class PQ2[A](
    private val prio: Int,
    head: A,
    private val lowerPrio: List[(Int, List[(Int, A)])]
) {
  def enqueue(prio: Int, head: A): PQ2[A] = {
    if (prio < this.prio) {
      PQ2(
        prio,
        head,
        ((prio + this.prio) / 2, (this.prio, this.head) :: Nil) :: lowerPrio
      )
    } else {
      lowerPrio match {
        case (h0, h1) :: t if prio >= h0 =>
          copy(lowerPrio = PQ2._enqueue(prio, head, h0, h1, t))
        case _ =>
          if (prio / 2 >= this.prio)
            copy(
              lowerPrio = PQ2._enqueue(
                prio,
                head,
                2 * this.prio,
                Nil,
                lowerPrio
              )
            )
          copy(
            lowerPrio = (
              (prio + this.prio) / 2,
              (prio, head) :: Nil
            ) :: lowerPrio
          )
      }
    }
  }

  def tail: Option[PQ2[A]] = PQ2._merge(lowerPrio)
}

object PQ2 {
  private def _enqueue[A](
      prio: Int,
      value: A,
      threshold: Int,
      head: List[(Int, A)],
      tail: List[(Int, List[(Int, A)])]
  ): List[(Int, List[(Int, A)])] = {
    tail match {
      case (head0, head1) :: next if prio >= head0 =>
        (threshold, head) :: _enqueue(
          prio,
          value,
          head0,
          head1,
          next
        )
      case _ =>
        if (prio / 2 >= threshold)
          (threshold, head) :: _enqueue(
            prio,
            value,
            threshold * 2,
            Nil,
            tail
          )
        else
          (threshold, (prio, value) :: head) :: tail
    }
  }

  private def _merge[A](
      lower: List[(Int, List[(Int, A)])]
  ): Option[PQ2[A]] = {
    lower match {
      case Nil => None
      case (_, stack) :: tail =>
        stack.reverse match {
          case Nil => _merge(tail)
          case (head0, head1) :: next =>
            Some(next.foldLeft(PQ2(head0, head1, tail)) {
              case (q, (p, a)) => q.enqueue(p, a)
            })
        }
    }
  }
}

/*
 * Could updates of the threshold here cause trouble?
 * Upward updates could cause higher priority tasks
 * that were inserted before the update to get stuck behind lower
 * priority tasks inserted after. Downward updates could cause
 * higher priority tasks that were inserted after the update to
 * get stuck behind lower priority tasks inserted before. The
 * latter is less bad, but still wrong.
 *
 * Rather than lower bounds, how about using upper bounds?
 */

class PQ3[A] private (
    private val prio: Int,
    val head: A,
    private val lower: PQ3.Stacks[A]
) {
  def enqueue(prio: Int, value: A): PQ3[A] =
    if (prio < this.prio) {
      /*
       * we have to be careful where we put the highest priority elements
       * perfomance could fall if too many buckets are created this way
       *
       * things in 'lower' could have the same priority as 'head'
       *
       */
      val _lower = lower match {
        case Nil =>
          PQ3._enqueue(this.prio, head, Nil)
        case (_, stack) :: next =>
          stack.foldRight(
            PQ3._enqueue(this.prio, head, next)
          ) {
            case ((i, a), q) => PQ3._enqueue[A](i, a, q)
          }
      }
      new PQ3(prio, value, _lower)
    } else {
      new PQ3(
        this.prio,
        this.head,
        PQ3._enqueue(prio, value, lower)
      )
    }

  def tail: Option[PQ3[A]] = PQ3._merge(lower)
}

object PQ3 {
  def apply[A](prio: Int, value: A) =
    new PQ3[A](prio, value, Nil)

  private type Stacks[A] = List[(Int, List[(Int, A)])]
  private def _enqueue[A](
      prio: Int,
      value: A,
      lower: Stacks[A]
  ): Stacks[A] =
    lower match {
      case Nil => (prio, (prio, value) :: Nil) :: Nil
      case (upperBound, stack) :: tail =>
        if (prio > upperBound) {
          (upperBound, stack) :: _enqueue(prio, value, tail)
        } else if (prio > upperBound / 2) {
          (upperBound, (prio, value) :: stack) :: tail
        } else {
          (prio, (prio, value) :: Nil) :: lower
        }
    }

  private def _merge[A](lower: Stacks[A]): Option[PQ3[A]] =
    lower match {
      case Nil => None
      case (_, pairs) :: next =>
        pairs.reverse match {
          case Nil => _merge(next)
          case (prio, value) :: next2 =>
            Some(next2.foldLeft(new PQ3(prio, value, next)) {
              case (q, (p, v)) => q.enqueue(p, v)
            })
        }
    }
}

/*
 * The solution becomes much simpler.
 *
 * despite the break up in smaller buckets
 * which is literally the only place where we
 * activity take advantage of the structur of integers.
 * I do wonder if this will turn out to be performant.
 *
 * Inserting lesser elements is a problem,
 * whatcha gonna do about it?
 *
 *
 * I wanna get back to the idea of just using a binary tree.
 *
 */
case class PQ4[A] private (
    index: Int,
    head: A,
    private val buckets: Array[List[PQ4[A]]]
) {
  //private val stack: Array[List[PQ4[A]]] = new Array(PQ4._index(index,0)+1)

  def enqueue(index: Int, value: A): PQ4[A] =
    PQ4.merge(this, PQ4(index, value))

  def tail: Option[PQ4[A]] = {
    val indexed = Iterator
      .iterate(index)(_ / 2 - 1)
      .zip(buckets)
      .filterNot {
        case (_, stack) => stack == null
      }
      .toList
    indexed match {
      case Nil => None
      case (_, head1) :: next =>
        PQ4.mergeAll(head1.reverse).map { result =>
          next.foreach {
            case (i, stack) =>
              val index = PQ4._index(result.index, i)
              val bucket = result.buckets(index)
              if (bucket == null) {
                result.buckets(index) = stack
              } else {
                result.buckets(index) = stack ++ bucket
              }
          }
          result
        }
    }
  }
}

object PQ4 {
  case class Q[+A](in: List[A], out: List[A])

  def apply[A](index: Int, value: A): PQ4[A] =
    PQ4(index, value, new Array(_index(index, 0) + 1))

  def merge[A](x: PQ4[A], y: PQ4[A]): PQ4[A] = {
    if (y.index < x.index) {
      merge(y, x)
    } else {
      val buckets = x.buckets.clone()
      val index = _index(x.index, y.index)
      val bucket = buckets(index)
      if (bucket == null) {
        buckets(index) = y :: Nil
      } else {
        buckets(index) ::= y
      }
      x.copy(buckets = buckets)
    }
  }

  def mergeAll[A](x: List[PQ4[A]]): Option[PQ4[A]] =
    x match {
      case Nil          => None
      case head :: next => Some(next.foldLeft(head)(merge[A]))
    }

  private def _index(x: Int, y: Int): Int = {
    @tailrec def _helper(x: Int, y: Int, z: Int): Int = {
      if (x == y) z
      else if (x > y) _helper(x / 2 - 1, y, z + 1)
      else _helper(x, y / 2 - 1, z)
    }
    _helper(x, y, 0)
  }
}

/*
 * Mutable example
 *
 *
 */
class PQ5[A] {
  def enqueue(prio: Int, a: A): Unit = {
    node.enqueue(prio, a)
  }

  def peek(): Option[A] = node.peek

  private val node = new PQ5.Node[A](32)
}

object PQ5 {
  class Q[A](var out: List[A] = Nil, var in: List[A] = Nil) {
    def enqueue(a: A) = {
      if (out == Nil) {
        out = a :: Nil
      } else {
        in ::= a
      }
    }
    def peek: Option[A] = {
      out match {
        case Nil => {
          out = in.reverse
          in = Nil
          out.headOption
        }
        case head :: _ =>
          Some(head)
      }
    }
    def dequeue: Option[A] = {
      out match {
        case Nil => {
          in.reverse match {
            case Nil => None
            case head :: next =>
              out = next
              in = Nil
              Some(head)
          }
        }
        case head :: next =>
          out = next
          Some(head)
      }
    }

    def isEmpty = in == Nil && out == Nil
  }

  private class Node[A](power: Int) {
    private val queue = new Q[A]
    //watch out for off-by-one
    private val buckets = new Array[Node[A]](power)
    private var minimum = power

    def enqueue(prio: Int, a: A): Unit =
      if (prio == 0) {
        queue.enqueue(a)
        minimum = -1
      } else {
        val log = _log2(prio)
        if (log < minimum) {
          minimum = log
        }
        if (buckets(log) == null) {
          buckets(log) = new Node(log)
        }
        buckets(log).enqueue(prio % (1 << log), a)
      }

    def peek: Option[A] =
      if (minimum < 0) queue.peek
      else buckets(minimum).peek

    def dequeue: Option[A] =
      if (minimum < 0) {
        val dq = queue.dequeue
        if (queue.isEmpty) {
          minimum = 0
        }
        searchMinimum()
        dq
      } else {
        val dq = buckets(minimum).dequeue
        searchMinimum()
        dq
      }

    private def searchMinimum(): Unit =
      while (minimum < power && (buckets(minimum) == null || buckets(
               minimum
             ).isEmpty)) {
        minimum += 1
      }

    def isEmpty = minimum == power
  }

  def _log2(int: Int): Int = {
    var i = int
    var j = 0
    var k = 0
    var h = 1 << 4
    do {
      k = i >> h
      if (k > 0) {
        i = k
        j += h
      }
      h >>= 1
    } while (h > 0)
    j
  }
}

/*
 * Replace immutable with a constant space edition?
 * 
 * Okay, I have to think about stashing arrays that are
 * no longer relevant.
 */

class PQ6[A] private (node: PQ6.Node[A]) {
  def enqueue(prio: Int, value: A): PQ6[A] =
    new PQ6(PQ6.merge(node, PQ6.Node(prio, value)))
  def head: Option[A] =
    if (node == null) None else Some(node.value)
  def tail: PQ6[A] =
    if (node == null)
      PQ6.empty[A]
    else
      new PQ6(
        node.buckets
          .foldRight[PQ6.Node[A]](null) {
            _.foldLeft(_)(PQ6.merge[A])
          }
      )
}

object PQ6 {
  private val _empty = new PQ6[Nothing](null)
  def empty[A]: PQ6[A] = _empty.asInstanceOf[PQ6[A]]
  private case class Node[A](
      prio: Int,
      value: A,
      offset: Int = 0,
      buckets: Array[List[Node[A]]] = null
  )

  private def merge[A](a: Node[A], b: Node[A]): Node[A] =
    if (a == null) b
    else if (b == null) a
    else if (b.prio < a.prio)
      insert(b, PQ5._log2(a.prio - b.prio + 1), a)
    else
      insert(a, PQ5._log2(b.prio - a.prio + 1), b)

  private def insert[A](
      a: Node[A],
      index: Int,
      b: Node[A]
  ): Node[A] = {
    if (a.buckets == null) {
      a.copy(offset = b.prio, buckets = Array(b :: Nil))
    } else if (index < a.offset) {
      val buckets = new Array[List[Node[A]]](
        index - a.offset + a.buckets.length
      )
      buckets(0) = b :: Nil
      Array.copy(
        a.buckets,
        0,
        buckets,
        a.offset - index,
        a.buckets.length
      )
      a.copy(offset = index, buckets = buckets)
    } else if (index >= a.offset + a.buckets.length) {
      val buckets =
        new Array[List[Node[A]]](index + 1 - a.offset)
      buckets(index - a.offset) = b :: Nil
      a.copy(buckets = buckets)
    } else {
      val buckets = a.buckets.clone()
      val bucket = buckets(index - a.offset)
      buckets(index - a.offset) =
        b :: (if (bucket == null) Nil else bucket)
      a.copy(buckets = buckets)
    }
  }
}

/*
 * Would this reach the high performance?
 */

class PQ7[A] private (
    private val prio: Int,
    val head: A,
    private val stack: List[PQ7[A]]
) {
  def enqueue(prio: Int, value: A): PQ7[A] =
    PQ7._merge(this, PQ7(prio, value))
  def tail = PQ7._tail(prio, stack)
}

object PQ7 {
  def apply[A](prio: Int, value: A): PQ7[A] =
    new PQ7(prio, value, Nil)

  private def _tail[A](
      prio: Int,
      stack: List[PQ7[A]]
  ): PQ7[A] = {
    stack match {
      case Nil           => null
      case a :: Nil      => a
      case a :: b :: Nil => _merge(b, a)
      case other =>
        val array = new Array[PQ7[A]](32)
        var min = 32
        var _stack = other
        while (_stack.nonEmpty) {
          val _head = _stack.head
          _stack = _stack.tail
          val index =
            Integer.numberOfLeadingZeros(_head.prio - prio)
          if (index < min) min = index
          array(index) = _merge(_head, array(index))
        }
        var result = array(min)
        while (min < 31) {
          min += 1
          result = _merge(result, array(min))
        }
        result
    }
  }
  private def _merge[A](a: PQ7[A], b: PQ7[A]): PQ7[A] =
    if (b == null) a
    else if (b.prio < a.prio)
      new PQ7[A](b.prio, b.head, a :: b.stack)
    else new PQ7[A](a.prio, a.head, b :: a.stack)
}

class PQ8[A] private (
    private val prio: Int,
    val head: A,
    private val array: Array[List[PQ8[A]]]
) {
  def enqueue(prio: Int, value: A): PQ8[A] = PQ8(prio, value)
  def tail: PQ8[A] = PQ8._tail(array)
}

object PQ8 {
  private val _array: Array[Nil.type] =
    Array.fill(7)(Nil)

  def apply[A](prio: Int, value: A) =
    new PQ8(
      prio,
      value,
      _array.asInstanceOf[Array[List[PQ8[A]]]]
    )

  private def _tail[A](array: Array[List[PQ8[A]]]): PQ8[A] =
    array.foldRight[PQ8[A]](null)(
      _.foldLeft(_)((b, c) => _merge(c, b))
    )

  private def _merge[A](a: PQ8[A], b: PQ8[A]): PQ8[A] =
    if (a == null) b
    else if (b == null) a
    else if (b.prio < a.prio)
      _insert(b, _index(b.prio - a.prio), a)
    else
      _insert(a, _index(a.prio - b.prio), b)

  private def _insert[A](
      a: PQ8[A],
      i: Int,
      b: PQ8[A]
  ): PQ8[A] = {
    val array = a.array.clone()
    array(i) ::= b
    new PQ8(a.prio, a.head, array)
  }

  private def _index(prio: Int): Int = {
    if (prio >> 1 == 0) prio
    else if (prio >> 2 == 0) 2
    else if (prio >> 4 == 0) 3
    else if (prio >> 8 == 0) 4
    else if (prio >> 16 == 0) 5
    else 6
  }
}
