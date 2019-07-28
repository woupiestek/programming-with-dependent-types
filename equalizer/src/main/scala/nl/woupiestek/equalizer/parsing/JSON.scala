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
    def number(str: String): B
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
      //here we are overriding the character stream itself,
      //which is why we cannot let the input do this for us.
      //escape sequences are like embedded pops...

      //adding a feature for modestly rewriting the input stream,
      //like removing and adding a few symbols,
      //that could help here.

      lazy val _string: A[List[Char]] =
        input.readIfEqual('\"', input.pop.map(_ => List.empty[Char])) <+>
          input.readIfEqual(
            '\\',
            _escape('"', '"') <+>
              _escape('\\', '\\') <+>
              _escape('/', '/') <+>
              _escape('b', '\b') <+>
              _escape('f', '\f') <+>
              _escape('n', '\n') <+>
              _escape('r', '\r') <+>
              _escape('t', '\t') <+>
              _unicode
          ) <+>
          input.read(c => _string.map(c :: _))

      def _escape(c0: Char, c1: Char) =
        input.readIfEqual(c0, _string.map(c1 :: _))

      lazy val _unicode: A[List[Char]] = {

        val _hex =
          (('0' to '9').map(c => c -> -'0') ++
            ('A' to 'Z').map(c => c -> (c - 'A' + 10)) ++
            ('a' to 'z').map(c => c -> (c - 'a' + 10))).toList

        def readHex[Z](next: (Int => A[List[Char]])): A[List[Char]] = {
          _hex.foldMap { case (c, i) => input.readIfEqual(c, next(i)) }
        }

        input.readIfEqual(
          'u',
          readHex(
            a =>
              readHex(
                b =>
                  readHex(
                    c =>
                      readHex { d =>
                        val e =
                          ((a << 12) + (b << 8) + (c << 4) + d).toChar
                        _string.map(e :: _)
                      }
                  )
              )
          )
        )

      }

      input.readIfEqual('"', _string.map(chars => Text(chars.mkString)))
    }

    val number: A[Token] = {
      def _digits(next: A[List[Char]]): A[List[Char]] =
        ('0' to '9').toList
          .foldMap(d => input.readIfEqual(d, _digits(next))) <+>
          next

      val __exponential = input.readIfEqual('+', _digits(input.pop)) <+>
        input.readIfEqual('-', _digits(input.pop)) <+>
        _digits(input.pop)

      val _exponential: A[List[Char]] =
        input.readIfEqual('e', __exponential) <+>
          input.readIfEqual('E', __exponential) <+>
          input.pop

      val _fraction: A[List[Char]] =
        input.readIfEqual('.', _digits(_exponential)) <+>
          input.pop

      val _uint: A[List[Char]] =
        input.readIfEqual('0', _fraction) <+>
          ('1' to '9').toList
            .foldMap(d => input.readIfEqual(d, _digits(_fraction)))

      val _int: A[List[Char]] = _uint <+> input.readIfEqual(
        '-',
        _uint
      )

      _int.map(chars => Text(chars.mkString))
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

      number <+> string <+>
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
