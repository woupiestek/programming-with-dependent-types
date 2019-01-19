package nl.woupiestek.equalizer

import scalaz.{BindRec, Maybe, Monad, \/}

final case class EndingStream[A](unfold: () => Maybe[(A, EndingStream[A])])

object EndingStream {

  def nil[A]: EndingStream[A] = EndingStream[A](() => Maybe.empty)

  def cons[A](head: => A, tail: => EndingStream[A]): EndingStream[A] =
    EndingStream[A](() => Maybe.just((head, tail)))

  def ana[A, B](f: A => Maybe[(B, A)])(a: A): EndingStream[B] =
    f(a).cata({ case (b, c) => cons(b, ana(f)(c)) }, nil)

  def concat[A](x: EndingStream[A], y: EndingStream[A]): EndingStream[A] =
    x.unfold().cata({ case (h, t) => cons(h, concat(t, y)) }, y)

  val instance: Monad[EndingStream] with BindRec[EndingStream] =
    new Monad[EndingStream] with BindRec[EndingStream] {
      override def tailrecM[A, B](f: A => EndingStream[A \/ B])(a: A): EndingStream[B] =
        bind(f(a))(_.fold(tailrecM(f), point(_)))

      override def point[A](a: => A): EndingStream[A] = cons(a, nil)

      override def bind[A, B](fa: EndingStream[A])(f: A => EndingStream[B]): EndingStream[B] =
        fa.unfold().cata({ case (h, t) => concat(f(h), bind(t)(f)) }, nil)
    }

}
