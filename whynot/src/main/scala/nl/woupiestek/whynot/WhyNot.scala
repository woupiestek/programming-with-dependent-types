package nl.woupiestek.whynot

import scala.annotation.tailrec

sealed trait WhyNot[+A] {
  def map[B](f: A => B): WhyNot[B]

  def flatMap[B](f: (A) => WhyNot[B]): WhyNot[B]

  def filter(p: A => Boolean): WhyNot[A]

  def withFilter(p: A => Boolean): WhyNot[A] = filter(p)

  def ++[B >: A](whyNot: WhyNot[B]): WhyNot[B]

  def take(i: Int): WhyNot[A]

  import WhyNot._

  def +:[B >: A](b: B): WhyNot[B] = Suspend { c =>
    c(b); this
  }

  //blocking
  def foreach(c: A => Unit): Unit = new Sink[A](c).listen(this)

  //blocking
  def foldLeft[B](b0: B)(f: (B, A) => B): B =
    new StateMachine[A, B](b0)(f).listen(this)

  def toList: List[A] =
    foldLeft[List[A]](Nil)((l, a) => a :: l).reverse

  //non-blocking
  def scanLeft[B](b0: B)(f: (B, A) => B): WhyNot[B] =
    new Scanner[A, B](b0)(f).listen(this)

  def ap[B](f: WhyNot[(A) => B]): WhyNot[B] = (f, this) match {
    case (Suspend(run0), Suspend(run1)) =>
      Suspend[B] { c =>
        var fun: Option[A => B] = None
        var arg: Option[A] = None
        val tail = run1 { x =>
          arg = Some(x)
        } ap run0 { y =>
          fun = Some(y)
        }
        fun foreach { y =>
          arg foreach { x =>
            c(y(x))
          }
        }
        tail
      }
    case _ => Return
  }

  def until(f: A => Boolean): WhyNot[A] =
    new Until[A](f).listen(this)
}

object WhyNot {

  object Return extends WhyNot[Nothing] {
    override def map[B](f: (Nothing) => B): WhyNot[B] = this

    override def flatMap[B](
        f: (Nothing) => WhyNot[B]
    ): WhyNot[B] = this

    override def filter(
        p: (Nothing) => Boolean
    ): WhyNot[Nothing] = this

    override def ++[B >: Nothing](
        whyNot: WhyNot[B]
    ): WhyNot[B] = whyNot

    override def take(i: Int): WhyNot[Nothing] = this
  }

  case class Suspend[+A](run: (A => Unit) => WhyNot[A])
      extends WhyNot[A] {
    override def map[B](f: (A) => B): WhyNot[B] = Suspend[B] {
      c =>
        run(a => c(f(a))) map f
    }

    override def flatMap[B](f: (A) => WhyNot[B]): WhyNot[B] = {
      var wb1: WhyNot[B] = Return
      val wb2 = run(a => wb1 = f(a)).flatMap(f)
      wb1 ++ wb2
    }

    override def filter(p: (A) => Boolean): WhyNot[A] =
      Suspend[A] { c =>
        run { a =>
          if (p(a)) c(a)
        } filter p
      }

    override def ++[B >: A](whyNot: WhyNot[B]): WhyNot[B] =
      Suspend { c =>
        run(c) ++ whyNot
      }

    override def take(i: Int): WhyNot[A] = Suspend { c =>
      if (i > 0) run(c) take (i - 1) else Return
    }
  }

  //helpers for state and tail recursion
  trait Listener[-A, +B] {
    def listen(wa: WhyNot[A]): B
  }

  class Sink[-A](action: A => Unit) extends Listener[A, Unit] {
    @tailrec final def listen(wa: WhyNot[A]): Unit = {
      wa match {
        case Suspend(run) => listen(run(action))
        case Return       => ()
      }
    }
  }

  class StateMachine[-A, B](var state: B)(action: (B, A) => B)
      extends Listener[A, B] {
    @tailrec final def listen(wa: WhyNot[A]): B = wa match {
      case Suspend(run) =>
        listen(run(a => state = action(state, a)))
      case Return => state
    }
  }

  class Scanner[-A, B](var state: B)(action: (B, A) => B)
      extends Listener[A, WhyNot[B]] {
    final def listen(wa: WhyNot[A]): WhyNot[B] = wa match {
      case Suspend(run) =>
        state +: listen(run(a => state = action(state, a)))
      case Return => state +: Return
    }
  }

  class Until[A](f: A => Boolean)
      extends Listener[A, WhyNot[A]] {
    private var stillListening = true

    final def listen(wa: WhyNot[A]): WhyNot[A] = wa match {
      case Suspend(run) if stillListening =>
        Suspend[A] { action =>
          listen(run { a =>
            if (f(a)) stillListening = false else action(a)
          })
        }
      case _ => Return
    }
  }

}
