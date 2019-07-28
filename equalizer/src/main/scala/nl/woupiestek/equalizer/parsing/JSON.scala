package nl.woupiestek.equalizer.parsing

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.JSON._

class JSON[A[_]: Applicative, B](
    input: Input[Token, A],
    output: Output[B],
    error: Error[A]
) {

  private val _object: A[Map[String, B]] = {
    def keyed(key: List[Char]) =
      matchMap(
        Colon -> Apply[A].apply3(unescapeText(key), value, tail)(
          (k, v, m) => m + (k -> v)
        )
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
    case Text(value)   => unescapeText(value).map(output.string)
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

  private def unescapeText(value: List[Char]): A[String] = {
    val hex =
      (('0' to '9').map(c => c -> -'0') ++
        ('A' to 'Z').map(c => c -> (c - 'A' + 10)) ++
        ('a' to 'z').map(c => c -> (c - 'a' + 10))).toMap

    def helper(in: List[Char], out: A[List[Char]]): A[String] = in match {
      case Nil | '"' :: Nil => out.map(_.mkString)
      case h :: t =>
        h match {
          case '\\' =>
            t match {
              case '\\' :: u => helper(u, out.map('\\' :: _))
              case '\"' :: u => helper(u, out.map('\"' :: _))
              case 'b' :: u  => helper(u, out.map('\b' :: _))
              case 'f' :: u  => helper(u, out.map('\f' :: _))
              case 'n' :: u  => helper(u, out.map('\n' :: _))
              case 'r' :: u  => helper(u, out.map('\r' :: _))
              case 't' :: u  => helper(u, out.map('\t' :: _))
              case 'u' :: a :: b :: c :: d :: u
                  if hex.contains(a) && hex.contains(b) && hex
                    .contains(c) && hex.contains(d) =>
                val e = ((hex(a) << 12) + (hex(b) << 8) + (hex(c) << 4) + hex(
                  d
                )).toChar
                helper(u, out.map(e :: _))
              case _ => error.raise("invalid escape code")
            }
          case _ => helper(t, out.map(h :: _))
        }
    }
    helper(value.tail, List.empty[Char].point[A])
  }
}

object JSON {
  //todo: something with token position
  //that could be another method of `Input`: position of A[Position]
  //as data that cannot change the choice of parser, but that can enrich the
  //parsing results.
  //this wouldn't allow putting positions in error messages however...

  sealed abstract class Token(val kind: String)
  final case class Text(value: List[Char]) extends Token("string")
  final case class Number(value: List[Char]) extends Token("number")
  final case object True extends Token("true") //tokenizer could know from the lack of quotes...
  final case object False extends Token("false")
  final case object Null extends Token("null")
  final case object LeftBracket extends Token("[")
  final case object RightBracket extends Token("]")
  final case object LeftBrace extends Token("{")
  final case object RightBrace extends Token("}")
  final case object Colon extends Token(":")
  final case object Comma extends Token(",")

  trait Input[I, A[_]] {
    def read[O](next: I => A[O]): A[O]
  }

  trait Output[B] {
    def `object`(elts: Map[String, B]): B
    def string(str: String): B
    def boolean(bool: Boolean): B
    def array(b: List[B]): B
    def `null`: B
    def number(value: List[Char]): B
  }

  trait Error[A[_]] {
    def raise[X](message: String): A[X]
  }

  trait LexInput[I, A[_]] extends Input[I, A] {
    def readIfEqual[O](guard: I, next: => A[O]): A[O]
    def pop: A[List[I]]
  }

  class Tokenizer[A[_]: PlusEmpty: Functor, B](
      input: LexInput[Char, A]
  ) {
    implicit def monoid[X]: Monoid[A[X]] = PlusEmpty[A].monoid[X]

    val string: A[Token] = {
      //leave the escapes alone for now!
      lazy val _string: A[List[Char]] =
        input.readIfEqual('\"', input.pop) <+>
          input.readIfEqual('\\', input.read(_ => _string)) <+>
          input.read(_ => _string)

      input.readIfEqual('"', _string.map(Text(_)))
    }

    val number: A[Token] = {
      def _digits(next: A[List[Char]]): A[List[Char]] =
        ('0' to '9').toList
          .foldMap(d => input.readIfEqual(d, _digits(next))) <+>
          next

      val exponential: A[List[Char]] = {
        val signed =
          input.readIfEqual('+', _digits(input.pop)) <+>
            input.readIfEqual('+', _digits(input.pop)) <+>
            _digits(input.pop)
        input.readIfEqual('e', signed) <+>
          input.readIfEqual('E', signed) <+>
          input.pop
      }

      val fraction: A[List[Char]] =
        input.readIfEqual('.', _digits(exponential)) <+> exponential

      val int: A[List[Char]] = {
        val uint = input.readIfEqual('0', input.pop) <+>
          ('1' to '9').toList.foldMap(input.readIfEqual(_, _digits(fraction)))
        (input.readIfEqual('-', uint) <+> uint)
      }

      int.map(Number(_))
    }

    def keyword(chars: String, token: Token): A[Token] =
      chars.toList
        .foldRight[A[Token]](input.pop.map(_ => token))(
          input.readIfEqual(_, _)
        )

    def symbol(char: Char, token: Token) =
      input.readIfEqual(char, input.pop.map(_ => token))

    private lazy val _whitespace: A[Unit] =
      List(' ', '\n', '\r', '\t').foldMap(
        s => input.readIfEqual(s, _whitespace)
      ) <+> input.pop.map(_ => ())

    val token: A[Token] = {
      string <+>
        number <+>
        keyword("false", False) <+>
        keyword("null", Null) <+>
        keyword("true", True) <+>
        symbol('[', LeftBracket) <+>
        symbol(']', RightBracket) <+>
        symbol('{', LeftBrace) <+>
        symbol('}', RightBrace) <+>
        symbol(':', Colon) <+>
        symbol(',', Comma)
    }
  }

}
