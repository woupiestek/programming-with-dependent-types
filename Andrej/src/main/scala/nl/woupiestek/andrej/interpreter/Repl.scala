package nl.woupiestek.andrej.interpreter

import nl.woupiestek.andrej.Free.{ Bind, Return }
import nl.woupiestek.andrej.Free

import scala.io.StdIn

object REPL {

  sealed trait Command[+T]

  case class Write(message: String) extends Command[Unit]

  case class Read(prompt: String) extends Command[String]

  case class While[T](cond: T => Boolean, program: T => Program[T], t: T) extends Command[T]

  type Program[T] = Free[Command, T]

  def apply[T](program: Program[T]): T = program match {
    case Return(t) => t
    case Bind(Write(msg), c) => apply(c(println(msg)))
    case Bind(Read(prompt), c) => apply(c(StdIn.readLine(prompt)))
    case Bind(While(c, p, t), d) => apply(d(if (c(t)) apply(p(t)) else t))
  }

  def write(msg: => String): Program[Unit] = Free.lift(Write(msg))

  def read(prompt: => String): Program[String] = Free.lift(Read(prompt))

  def doWhile[T](c: T => Boolean, p: T => Program[T])(t: T): Program[T] = Free.lift(While[T](c, p, t))
}

object TrivialREPL extends App {

  import REPL._

  for {
    _ <- write("Welcome to the REPL")
    x <- read("Q:")
    _ <- doWhile[String](input => !input.equalsIgnoreCase("exit"),
      input => for {
        _ <- write(input)
        y <- read("Q:")
      } yield y)(x)
    _ <- write("bye bye")
  } yield ()

}