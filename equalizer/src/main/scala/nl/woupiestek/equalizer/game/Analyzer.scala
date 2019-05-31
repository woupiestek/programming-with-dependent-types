package nl.woupiestek.equalizer.game

import scala.annotation.tailrec
import scalaz.{Value => _, _}
import Scalaz._
import Lambda._

object Analyzer {

  type System = Option[List[(Pattern, Pattern, Set[Value])]]

  final case class Sequent(eqs: System, ante: Set[Sequent], args: Set[Value])

  sealed abstract class Vars[X] extends (Int => (X, Int))
  implicit val isMonadRec: Monad[Vars] with BindRec[Vars] = new Monad[Vars]
  with BindRec[Vars] {
    def point[A](a: => A): Vars[A] = new Vars[A] {
      def apply(v1: Int): (A, Int) = (a, v1)
    }
    def bind[A, B](fa: Vars[A])(f: A => Vars[B]): Vars[B] = new Vars[B] {
      def apply(v1: Int): (B, Int) = {
        val (a, i) = fa(v1)
        f(a)(i)
      }
    }
    def tailrecM[A, B](f: A => Vars[A \/ B])(a: A): Vars[B] = new Vars[B] {
      def apply(v1: Int): (B, Int) = {
        @tailrec def helper(c: A, i: Int): (B, Int) =
          f(c)(i) match {
            case (-\/(d), j) => helper(d, j)
            case (\/-(d), j) => (d, j)
          }
        helper(a, v1)
      }
    }
  }
  def mark(name: String): Vars[Value] = new Vars[Value] {
    def apply(v1: Int): (Value, Int) = (Value(name, v1), 1 + v1)
  }

  def equate(
    in: List[(Lambda, Lambda, Set[Value])],
    out: List[(Pattern, Pattern, Set[Value])]
): Vars[Option[List[(Pattern, Pattern, Set[Value])]]] =
  in match {
    case Nil => Monad[Vars].point(Some(out))
    case h :: t =>
      h match {
        case (Pattern(lx, ly), Pattern(rx, ry), vs) =>
          if (lx == rx && ly.length == ry.length)
            equate(ly.zip(ry).map {
              case (l, r) => (l(Nil), r(Nil), vs)
            } ++ t, out)
          else if (vs(lx) || vs(rx)) Monad[Vars].point(None)
          else equate(t, (Pattern(lx, ly), Pattern(rx, ry), vs) :: out)
        case (l, r, vs) =>
          for {
            v <- mark("$")
            s = Pattern(v, Nil) :: Nil
            y <- equate((l(s), r(s), vs + v) :: t, out)
          } yield y
      }
  }

  def analyze(sentence: Sentence): Vars[Sequent] = {
    def helper(
        in: List[(Sentence, Set[Sentence], Set[Value])],
        out: List[
          (
              Sentence,
              System,
              Set[Sentence],
              Set[Value]
          )
        ]
    ): Vars[Sequent] = in match {
      case Nil =>
        Monad[Vars].point(out.foldLeft(Map.empty[Sentence, Sequent]) {
          case (o, (s, qs, as, vs)) => o + (s -> Sequent(qs, as.map(o), vs))
        }(sentence))
      case (h, antes, values) :: t =>
        h match {
          case Equation(left, right) =>
            val heap = values.groupBy(_.name).map {
              case (k, v) => k -> Pattern(v.maxBy(_.offset), Nil)
            }
            equate(
              (
                evaluate(left, heap, Nil),
                evaluate(right, heap, Nil),
                Set.empty[Value]
              ) :: Nil,
              Nil
            ).flatMap(qs => helper(t, ((h, qs, antes, values)) :: out))
          case Generalization(varName, body) =>
            mark(varName).flatMap(
              v => helper((body, antes, values + v) :: in, out)
            )
          case Implication(ante, con) =>
            helper(
              (ante, Set.empty[Sentence], values) :: (con, antes + ante, values) :: in,
              out
            )
        }
    }

    helper(((sentence, Set.empty[Sentence], Set.empty[Value])) :: Nil, Nil)
  }

}
