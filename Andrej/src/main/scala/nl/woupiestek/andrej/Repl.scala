package nl.woupiestek.andrej

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }
import scala.io.StdIn.readLine
import scala.util.{ Failure, Success }

object Repl extends App {

  private def rep(input: String)(implicit ec: ExecutionContext): Future[String] = Future {
    Parboil(input) match {
      case Success(expr) => PrettyPrint(WeakHeadNormalize(expr))
      case Failure(ex) => ex.getMessage
    }
  }

  private def loop(): Unit = {
    val input = readLine
    if ("exit" != input) {
      rep(readLine) map println
      loop()
    }
  }

  println("Welcome to the Andrej REPL")
  loop()
  println("bye bye")

}
