package nl.woupiestek.andrej.interpreter

import nl.woupiestek.andrej.typeclasses.Monad
import Monad._
import scala.language.higherKinds

import scala.io.StdIn

trait REPL[IO[_]] {
  def write(message: String): IO[Unit]

  def read(prompt: String): IO[String]

  def doWhile[T](condition: T => IO[Boolean], program: T => IO[T], init: T): IO[T]
}

object REPL {

  def write[IO[_]](message: String)(implicit IO: REPL[IO]): IO[Unit] = IO.write(message)

  def read[IO[_]](prompt: String)(implicit IO: REPL[IO]): IO[String] = IO.read(prompt)

  implicit class Looper[IO[_], T](t: T)(implicit IO: REPL[IO] with Monad[IO]) {
    def doWhile(c: (T) => IO[Boolean], p: (T) => IO[T]): IO[T] = IO.doWhile(c, p, t)
  }
}

object TrivialREPL extends App {

  import REPL._

  type Program[T] = Unit => T

  implicit val console = new REPL[Program] with Monad[Program] {
    override def write(message: String): Program[Unit] = unit(println(message))

    override def read(prompt: String): Program[String] = unit(StdIn.readLine(prompt))

    override def doWhile[T](c: (T) => Program[Boolean], p: (T) => Program[T], t: T): Program[T] = {
      def fixand(u: T): T = if (c(u).apply(())) fixand(p(u).apply(())) else u

      _ => fixand(t)
    }

    override def unit[A](a: A): Program[A] = _ => a

    override def bind[A, B](fa: Program[A])(f: (A) => Program[B]): Program[B] = f(fa.apply(()))
  }

  val program: Program[Unit] = for {
    _ <- write("Welcome to the REPL")
    x <- read("Q:")
    _ <- x.doWhile(
      input => console.unit(!input.equalsIgnoreCase("exit")),
      input => for {
        _ <- write(input)
        y <- read("Q:")
      } yield y)
    _ <- write("bye bye")
  } yield ()

  program.apply(())

}