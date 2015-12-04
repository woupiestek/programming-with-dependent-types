package nl.woupiestek.andrej.interpreter

import scala.io.StdIn

trait REPL[+T] {

  import REPL._

  def flatMap[U](f: T => REPL[U]): REPL[U]

  def map[U](f: T => U): REPL[U] = flatMap(x => Return(f(x)))

  def withFilter(c: T => Boolean): REPL[T] = BreakIf(this, c)

  def filter(c: T => Boolean) = withFilter(c)

  def stop = flatMap { case () => Break }
}

object REPL {

  case class Print(message: String) extends REPL[Unit] {
    override def flatMap[U](f: (Unit) => REPL[U]): REPL[U] = {
      println(message)
      f()
    }
  }

  case class Return[T](t: T) extends REPL[T] {
    override def flatMap[U](f: (T) => REPL[U]): REPL[U] = f(t)
  }

  case class Read(prompt: String) extends REPL[String] {
    override def flatMap[U](f: (String) => REPL[U]): REPL[U] = f(StdIn.readLine(prompt))
  }

  def loop[T](f: T => REPL[T])(t: T): REPL[T] = {
    f(t) match {
      case Break => Return(t)
      case y => y.flatMap(loop(f))
    }
  }

  case object Break extends REPL[Nothing] {
    override def flatMap[U](f: (Nothing) => REPL[U]): REPL[U] = this
  }

  case class BreakIf[T](x: REPL[T], c: T => Boolean) extends REPL[T] {
    override def flatMap[U](f: (T) => REPL[U]): REPL[U] = x.flatMap { y => if (c(y)) REPL.Break else f(y) }
  }

}

object TrivialREPL extends App {

  import REPL._

  Print("Welcome to the Andrej REPL").flatMap {
    loop[Unit] { case () => Read("Q:").withFilter("exit" == _).flatMap(Print) }
  }.flatMap { case () => Print("bye bye") }.stop

}