package nl.woupiestek.andrej

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }
import scala.io.StdIn.readLine
import scala.util.{ Failure, Success }

object Repl extends App {

  private def rep(input: String)(implicit ec: ExecutionContext): Future[String] = Future {
    Parboil(input) match {
      case Success(expr) => prettyPrint(WeakHeadNormalize(expr))
      case Failure(ex) => ex.getMessage
    }
  }

  def prettyPrint(expr: Expr[Any], depth: Int = 0): String = expr match {
    case Iden(None) => "x_" + depth
    case Iden(Some(y)) => prettyPrint(Iden(y), depth + 1)
    case Appl(x, y) => (x :: y).map(prettyPrint(_, depth)).mkString("(", " ", ")")
    case Abst(s, c) => s"\\:${prettyPrint(s, depth)}.${prettyPrint(c, depth + 1)}"
    case Prod(s) => "Prod" + prettyPrint(s, depth)
    case Type => "Type"
    case Shift(e) => prettyPrint(e, depth - 1)
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
