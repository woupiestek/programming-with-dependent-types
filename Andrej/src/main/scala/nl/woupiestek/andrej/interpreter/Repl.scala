package nl.woupiestek.andrej.interpreter

import nl.woupiestek.andrej.free.MonadRec
import nl.woupiestek.andrej.typeclasses.Monad._

import scala.io.StdIn
import scala.language.higherKinds
import scalaz.{ -\/, \/, \/- }

trait REPL[IO[_]] {
  def write(message: String): IO[Unit]

  def read(prompt: String): IO[String]

}

object REPL {

  def write[IO[_]](message: String)(implicit IO: REPL[IO]): IO[Unit] = IO.write(message)

  def read[IO[_]](prompt: String)(implicit IO: REPL[IO]): IO[String] = IO.read(prompt)

}

object TrivialREPL extends App {

  import REPL._

  type Program[T] = Unit => T

  implicit val console = new REPL[Program] with MonadRec[Program] {
    override def write(message: String): Program[Unit] = unit(println(message))

    override def read(prompt: String): Program[String] = unit(StdIn.readLine(prompt))

    override def unit[A](a: A): Program[A] = _ => a

    override def bind[A, B](fa: Program[A])(f: (A) => Program[B]): Program[B] = f(fa.apply(()))

    override def tailRec[A, B](f: (A) => Program[\/[A, B]]): (A) => Program[B] = {
      def run: A => B = a => f(a)(()) match {
        case -\/(a0) => run(a0)
        case \/-(b) => b
      }

      a => _ => run(a)
    }
  }

  val program: Program[Unit] = for {
    _ <- write("Welcome to the REPL")
    x <- read("Q:")
    _ <- console.tailRec[String, Unit](input =>
      if (input.equalsIgnoreCase("exit")) write("bye bye").map(\/-(_))
      else for {
        _ <- write(input)
        y <- read("Q:")
      } yield -\/(y))(x)
  } yield ()

  program(())

}