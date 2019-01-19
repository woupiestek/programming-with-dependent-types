package nl.woupiestek.equalizer

import scalaz.Comonad

final case class EndlessStream[A](head: () => A, tail: () => EndlessStream[A])

object EndlessStream {

  def cons[A](head: => A, tail: => EndlessStream[A]) = EndlessStream(() => head, () => tail)

  def ana[A, B](head: A => B, tail: A => A)(a: A): EndlessStream[B] =
    cons(head(a), ana(head, tail)(a))

  implicit val instance: Comonad[EndlessStream] = new Comonad[EndlessStream] {
    override def copoint[A](p: EndlessStream[A]): A = p.head()

    override def cobind[A, B](fa: EndlessStream[A])(f: EndlessStream[A] => B): EndlessStream[B] =
      ana(f, (_: EndlessStream[A]).tail())(fa)

    override def map[A, B](fa: EndlessStream[A])(f: A => B): EndlessStream[B] =
      cobind(fa)(x => f(x.head()))
  }

}