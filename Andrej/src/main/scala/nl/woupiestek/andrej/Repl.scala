package nl.woupiestek.andrej


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.StdIn.readLine
import scala.util.{Failure, Success}

/**
 * Created by Wouter on 9-10-2015.
 */
object Repl extends App {

  private def loop {
    val input = readLine
    if ("exit" != input) {
      Future {
        Parboil(input) match {
          case Success(expr) => println(PrettyPrint(WeakHeadNormalize(expr)))
          case Failure(ex) => println(ex.getMessage)
        }
      }
      loop
    }
  }

  println("Welcome to the Andrej REPL")
  loop
  println("bye bye")
}
