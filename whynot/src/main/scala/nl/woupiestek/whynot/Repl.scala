package nl.woupiestek.whynot

import nl.woupiestek.whynot.WhyNot._

import scala.io.StdIn
import scala.language.postfixOps

object Repl extends App {

  def fromSeq[A](seq: Seq[A]): WhyNot[A] = seq toList match {
    case Nil          => Return
    case head :: tail => head +: fromSeq(tail)
  }

  for (i <- fromSeq(1 to 10) if i % 2 == 0) print(i)
  println()

  val ij = for {
    i <- fromSeq(1 to 10)
    j <- fromSeq(1 to i)
  } yield i * j

  for (k <- ij take 20) print(k)
  println()

  val ap0 = fromSeq(1 to 5) map [Int => Int](
      i => (j: Int) => i - j
  )
  val ap1 = fromSeq(1 to 10) ap ap0
  for (k <- ap1) print(k)
  println()

  println(
    (for (i <- fromSeq(1 to 5).scanLeft(1)(_ * _))
      yield i).toList.mkString(" ")
  )

  def in: WhyNot[String] = Suspend[String] { c =>
    c(StdIn.readLine)
    in
  }

  for (line <- in until ("exit" equalsIgnoreCase)) println(line)
}
