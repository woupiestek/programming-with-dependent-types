package nl.woupiestek.andrej.free

import scala.language.higherKinds

trait ForEachFree[M[_], T] {

  import ForEachFree._

  def forEach(onReturn: T => Unit, onBind: Folder[M]): Unit

  def flatMap[U](f: T => ForEachFree[M, U]): ForEachFree[M, U]

  def map[U](f: T => U): ForEachFree[M, U] =
    flatMap(t => Return(f(t)))
}

object ForEachFree {

  trait Folder[M[_]] {
    def apply[U](m: M[U], f: U => Unit): Unit
  }

  case class Return[M[_], T](value: T)
      extends ForEachFree[M, T] {
    override def forEach(
        onReturn: (T) => Unit,
        onBind: Folder[M]
    ): Unit = onReturn(value)

    override def flatMap[U](
        f: (T) => ForEachFree[M, U]
    ): ForEachFree[M, U] = f(value)
  }

  case class Bind[M[_], U, T](
      request: M[U],
      callback: U => ForEachFree[M, T]
  ) extends ForEachFree[M, T] {

    override def forEach(
        onReturn: (T) => Unit,
        onBind: Folder[M]
    ): Unit = {
      val q = new Queue[ForEachFree[M, T]]
      onBind(request, (u: U) => q.enqueue(callback(u)))
      while (!q.isEmpty) {
        q.dequeue().forEach(onReturn, onBind)
      }
    }

    override def flatMap[V](
        f: (T) => ForEachFree[M, V]
    ): ForEachFree[M, V] =
      Bind[M, U, V](request, t => callback(t).flatMap(f))
  }

  private class Queue[T >: Null] {
    private var in: List[T] = Nil
    private var out: List[T] = Nil

    def enqueue(t: T): Unit = {
      in ::= t
    }

    def isEmpty: Boolean = in.isEmpty && out.isEmpty

    def dequeue(): T = {
      if (out.isEmpty) {
        out = in.reverse
        in = Nil
      }
      out match {
        case head :: tail =>
          out = tail
          head
        case Nil => null
      }
    }
  }

}
