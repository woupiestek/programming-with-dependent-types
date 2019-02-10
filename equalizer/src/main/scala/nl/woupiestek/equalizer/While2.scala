package nl.woupiestek.equalizer

import scalaz._
import scala.util.control.TailCalls._

sealed abstract class While2[X] {
  def fold[Y](f: X => Y)(g: (=> Y) => Y): Y
}

object While2 {

  def suspend[A](a: => While2[A]): While2[A] = new While2[A] {
    def fold[Y](f: A => Y)(g: (=> Y) => Y): Y = {
      g(a.fold(f)(g))
    }
  }

  //slow as hell, with no option to speed up.
  def extract[A](a: While2[A]): A = {
    //there is a way!
    a.fold(done)(tailcall).result
  }

  implicit val instance: Monad[While2] with BindRec[While2] =
    new Monad[While2] with BindRec[While2] {
      def point[A](a: => A): While2[A] = new While2[A] {
        def fold[Y](f: A => Y)(g: (=> Y) => Y): Y = f(a)
      }

      def bind[A, B](fa: While2[A])(fb: A => While2[B]): While2[B] =
        fa.fold(a => suspend(fb(a)))(suspend)

      def tailrecM[A, B](f: A => While2[A \/ B])(a: A): While2[B] = {
        f(a).fold {
          case -\/(b) => suspend(tailrecM(f)(b))
          case \/-(b) => point(b)
        }(suspend)
      }
    }
}
