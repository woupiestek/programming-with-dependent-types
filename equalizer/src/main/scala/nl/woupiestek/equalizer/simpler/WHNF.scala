package nl.woupiestek.equalizer.simpler

import nl.woupiestek.equalizer.simpler.BreakDown._
import scalaz._
import scalaz.Scalaz._
import scalaz.Free._
import nl.woupiestek.equalizer.simpler.BreakDown._

sealed abstract class WHNF

object WHNF {

  case class WApp(
      eqs: List[(Trampoline[WHNF], Trampoline[WHNF])],
      tail: List[Trampoline[WHNF]],
      head: String
  ) extends WHNF

  def app(
      eqs: List[(Trampoline[WHNF], Trampoline[WHNF])],
      tail: List[Trampoline[WHNF]],
      head: String
  ): Trampoline[WHNF] =
    WApp(eqs, tail, head).asInstanceOf[WHNF].pure[Trampoline]

  case class WAbs(
      head: String,
      body: BreakDown,
      heap: Map[String, Trampoline[WHNF]],
      eqs: List[(Trampoline[WHNF], Trampoline[WHNF])]
  ) extends WHNF

  def abs(
      head: String,
      body: BreakDown,
      heap: Map[String, Trampoline[WHNF]],
      eqs: List[(Trampoline[WHNF], Trampoline[WHNF])]
  ): Trampoline[WHNF] =
    WAbs(head, body, heap, eqs)
      .asInstanceOf[WHNF]
      .pure[Trampoline]

  def normalize(
      pivot: BreakDown,
      heap: Map[String, Trampoline[WHNF]]
  ): Trampoline[WHNF] = pivot match {
    case Iden(a)   => heap.getOrElse(a, app(Nil, Nil, a))
    case Abs(a, b) => abs(a, b, heap, Nil)
    case App(a, b) =>
      normalize(a, heap).flatMap {
        case WApp(c, d, e) => app(c, normalize(b, heap) :: d, e)
        case WAbs(c, d, e, f) =>
          normalize(d, heap ++ e + (c -> normalize(b, heap)))
            .flatMap {
              case WApp(g, h, i)    => app(f ++ g, h, i)
              case WAbs(g, h, i, j) => abs(g, h, i, f ++ j)
            }
      }
    case Let(a, b, c) =>
      normalize(c, heap + (a -> normalize(b, heap)))
    case Check(a, b, c) =>
      normalize(c, heap).flatMap {
        case WApp(d, e, f) =>
          app(
            (normalize(a, heap), normalize(b, heap)) :: d,
            e,
            f
          )
        case WAbs(d, e, f, g) =>
          abs(
            d,
            e,
            f,
            (normalize(b, heap), normalize(c, heap)) :: g
          )
      }
  }

}
