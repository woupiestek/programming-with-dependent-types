package nl.woupiestek.equalizer.parsing

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.JSON._

class JSON[A[_]: Applicative, B](
    input: Input[A],
    output: Output[B],
    error: Error[A]
) {

  private val _object: A[Map[String, B]] = {
    def keyed(key: String) = matchMap(
      Colon -> Apply[A].apply2(value, tail)((v, m) => m + (key -> v))
    )
    lazy val tail: A[Map[String, B]] = matchMap(
      RightBrace -> Map.empty[String, B].point[A],
      Comma -> input.read {
        case Text(key) => keyed(key)
        case other     => wrong(other, "string")
      }
    )

    input.read {
      case RightBrace => Map.empty[String, B].point[A]
      case Text(key)  => keyed(key)
      case other      => wrong(other, "string or }")
    }
  }

  private val _value: PartialFunction[Token, A[B]] = {
    case False         => output.boolean(false).point[A]
    case LeftBrace     => _object.map(output.`object`)
    case LeftBracket   => _array.map(output.array)
    case Null          => output.`null`.point[A]
    case Number(value) => output.number(value).point[A]
    case Text(value)   => output.string(value).point[A]
    case True          => output.boolean(true).point[A]
  }

  private val _array: A[List[B]] = {
    lazy val tail: A[List[B]] =
      matchMap(
        RightBracket -> List.empty[B].point[A],
        Comma -> Apply[A].apply2(value, tail)(_ :: _)
      )

    input.read(
      _value andThen ((head: A[B]) => Apply[A].apply2(head, tail)(_ :: _)) orElse {
        case RightBracket => List.empty[B].point[A]
        case other        => wrong(other, "boolean, [, ], {, null, number or string")
      }
    )
  }

  val value: A[B] = input.read(
    _value orElse {
      case other =>
        wrong(other, "boolean, [, {, null, number or string")
    }
  )

  private def matchMap[X](seq: (Token, A[X])*): A[X] = {
    val map = seq.toMap
    input.read(
      next => map.getOrElse(next, wrong(next, map.keys.mkString(" or ")))
    )
  }

  private def wrong[X](next: Token, needed: => String): A[X] =
    error.raise(s"$needed needed, but ${next.kind} found.")
}

object JSON {
  //todo: something with token position
  //that could be another method of `Input`: position of A[Position]
  //as data that cannot change the choice of parser, but that can enrich the
  //parsing results.
  //this wouldn't allow putting positions in error messages however...

  sealed abstract class Token(val kind: String)
  final case class Text(value: String) extends Token("string")
  final case class Number(value: String) extends Token("number")
  final case object True extends Token("true") //tokenizer could know from the lack of quotes...
  final case object False extends Token("false")
  final case object Null extends Token("null")
  final case object LeftBracket extends Token("[")
  final case object RightBracket extends Token("]")
  final case object LeftBrace extends Token("{")
  final case object RightBrace extends Token("]")
  final case object Colon extends Token(":")
  final case object Comma extends Token(",")

  trait Input[A[_]] {
    def read[O](next: Token => A[O]): A[O]
  }

  trait Output[B] {
    def `object`(elts: Map[String, B]): B
    def string(str: String): B
    def boolean(bool: Boolean): B
    def array(b: List[B]): B
    def `null`: B
    def number(str: String): B
  }

  trait Error[A[_]] {
    def raise[X](message: String): A[X]
  }

}
