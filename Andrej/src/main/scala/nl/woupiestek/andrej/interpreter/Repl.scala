package nl.woupiestek.andrej.interpreter

import nl.woupiestek.andrej.{ CallbackInterpreter, Free, StraightInterpreter }

import scala.annotation.tailrec
import scala.io.StdIn

object REPL {

  sealed trait Command[+T]

  case class Write(message: String) extends Command[Unit]

  case class Read(prompt: String) extends Command[String]

  case class While[T](cond: T => Boolean, program: T => Program[T], value: T) extends Command[T]

  type Program[T] = Free[Command, T]

  object Console extends StraightInterpreter[Command] {
    override def evaluate[T](ft: Command[T]): T = ft match {
      case Write(msg) => println(msg)
      case Read(prompt) => StdIn.readLine(prompt)
      case While(c: (T => Boolean), p: (T => Program[T]), t) => executeWhile(c, p, t)
    }

    @tailrec private def executeWhile[T](c: T => Boolean, p: T => Program[T], t: T): T =
      if (c(t)) executeWhile(c, p, execute(p(t))) else t
  }

  def apply[T](program: Program[T]): T = Console.execute(program)

  def write(msg: => String): Program[Unit] = Free.lift(Write(msg))

  def read(prompt: => String): Program[String] = Free.lift(Read(prompt))

  def doWhile[T](c: T => Boolean, p: T => Program[T])(t: T): Program[T] = Free.lift(While[T](c, p, t))
}

object TrivialREPL extends App {

  import REPL._

  val program = for {
    _ <- write("Welcome to the REPL")
    x <- read("Q:")
    _ <- doWhile[String](input => !input.equalsIgnoreCase("exit"),
      input => for {
        _ <- write(input)
        y <- read("Q:")
      } yield y)(x)
    _ <- write("bye bye")
  } yield ()

  Console.execute(program)

}