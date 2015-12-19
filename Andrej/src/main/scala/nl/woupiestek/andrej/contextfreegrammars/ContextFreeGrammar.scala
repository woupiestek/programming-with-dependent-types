package nl.woupiestek.andrej.contextfreegrammars

import scala.collection.mutable

class ContextFreeGrammar[Symbol](start: Symbol, produce: Set[(Symbol, List[Symbol])]) {

  case class Tree(root: Symbol, children: List[Tree] = Nil)

  type P[X] = Parser[Symbol, X]

  private def done[T](t: T): P[T] = Parser(None, Some(t))

  private def next[T](f: Symbol => P[T]): P[T] = Parser(Some(f), None)

  private def fail[T]: P[T] = Parser(None, None)

  private def collect(root: Symbol): Stream[List[Symbol]] = {
    produce collect { case (x, y) if x == root => y }
  }.toStream

  private def sequence(in: List[Symbol], out: P[List[Tree]] = done(Nil)): P[List[Tree]] = in match {
    case Nil => out
    case head :: tail => sequence(tail, for {
      x <- parser(head)
      y <- out
    } yield x :: y)
  }

  private def subparsers(root: Symbol, symbol: Symbol): P[List[Tree]] =
    if (root == symbol) done(Nil)
    else collect(root).foldLeft[P[List[Tree]]](fail) { (x, y) => x join sequence(y) }

  def parser(root: Symbol = start): P[Tree] = {
    if (memory contains root) memory(root)
    else {
      val p = next(subparsers(root, _)).map(Tree(root, _))
      memory += (root -> p)
      p
    }
  }

  private val memory: mutable.Map[Symbol, P[Tree]] = mutable.Map()

}
