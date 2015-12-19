package nl.woupiestek.andrej.interpreter

import scala.io.StdIn

trait REPL[+T] {

  import REPL._

  def flatMap[U](f: T => REPL[U]): REPL[U]

  def map[U](f: T => U): REPL[U] = flatMap(x => Return(f(x)))
}

object REPL {

  case class Return[T](t: T) extends REPL[T] {
    override def flatMap[U](f: (T) => REPL[U]): REPL[U] = f(t)
  }

  val unit: REPL[Unit] = Return()

  def print(message: String): REPL[Unit] = {
    println(message)
    unit
  }

  def read(prompt: String): REPL[String] = {
    Return(StdIn.readLine(prompt))
  }

  case object Break extends REPL[Nothing] {
    override def flatMap[U](f: (Nothing) => REPL[U]): REPL[U] = this
  }

  def loop[T](f: T => REPL[T])(t: T): REPL[T] = {
    f(t) match {
      case Break => Return(t)
      case y => y.flatMap(loop(f))
    }
  }

  def thunk(t: => REPL[Unit]): Unit => REPL[Unit] = {
    case () => t
  }

}

object TrivialREPL extends App {

  import REPL._

  print("Welcome to the Andrej REPL").flatMap {
    loop(thunk(read("Q:").flatMap {
      input => if ("exit" == input) Break else print(input)
    }))
  }.flatMap(thunk(print("bye bye")))
}