package nl.woupiestek.equalizer.parsing

import scalaz._
import Scalaz._
import nl.woupiestek.equalizer.parsing.JSON._

class JSON[B](
    output: Output[B]
) {

  lazy val _object: Parser3[Char, Either[String, B]] = {
    val _pair: Parser3[
      Char,
      (Either[String, (String, B)])
    ] = for {
      k <- string
      _ <- symbol(':')
      v <- value
    } yield k.flatMap(a => v.map(a -> _))

    lazy val _nonEmpty
        : Parser3[Char, Either[String, Map[String, B]]] =
      _pair.flatMap {
        case Right(p) =>
          symbol(',').flatMap(
            _ =>
              _nonEmpty
                .map(_.map((m: Map[String, B]) => m + p))
          ) ++
            symbol('}').map(_ => Right(Map(p))) ++
            Parser3.point(Left("expected ',' or '}'"))
        case Left(e) => Parser3.point(Left(e))
      }
    symbol('}').flatMap(
      _ =>
        _nonEmpty.map(_.map(output.`object`)) ++
          symbol('}')
            .map(_ => Right(output.`object`(Map.empty)))
    )
  }

  lazy val _array: Parser3[Char, Either[String, B]] = {
    lazy val tail: Parser3[Char, Either[String, List[B]]] =
      symbol(',').flatMap(
        _ =>
          value.flatMap(
            x => tail.map(t => x.flatMap(h => t.map(h :: _)))
          )
      ) ++
        symbol(']').map(_ => Right(List.empty[B]))

    symbol(']').flatMap(
      _ =>
        value.flatMap(
          x =>
            tail.map(
              y =>
                x.flatMap(h => y.map(t => output.array(h :: t)))
            )
        )
          ++
            symbol(']')
              .map(_ => Right(output.array(List.empty[B])))
    )
  }

  lazy val value: Parser3[Char, Either[String, B]] =
    string.map(_.map(output.string)) ++
      keyword("true").map(_ => Right(output.boolean(true))) ++
      keyword("false").map(_ => Right(output.boolean(false))) ++
      keyword("null").map(_ => Right(output.`null`)) ++
      number.map(chars => Right(output.number(chars.toList))) ++
      _object ++
      _array ++
      Parser3.point(
        Left("boolean, [, ], {, null, number or string")
      )

}

object JSON {

  private def unescapeText(
      value: IndexedSeq[Char]
  ): Either[String, String] = {
    val hex =
      (('0' to '9').map(c => c -> -'0') ++
        ('A' to 'Z').map(c => c -> (c - 'A' + 10)) ++
        ('a' to 'z').map(c => c -> (c - 'a' + 10))).toMap

    def helper(
        in: List[Char],
        out: List[Char]
    ): Either[String, String] = in match {
      case Nil | '"' :: Nil => Right(out.reverse.mkString)
      case h :: t =>
        h match {
          case '\\' =>
            t match {
              case '\\' :: u =>
                helper(u, '\\' :: out)
              case '\"' :: u =>
                helper(u, '\"' :: out)
              case 'b' :: u => helper(u, ('\b' :: out))
              case 'f' :: u => helper(u, ('\f' :: out))
              case 'n' :: u => helper(u, ('\n' :: out))
              case 'r' :: u => helper(u, ('\r' :: out))
              case 't' :: u => helper(u, ('\t' :: out))
              case 'u' :: a :: b :: c :: d :: u
                  if hex.contains(a) && hex
                    .contains(b) && hex
                    .contains(c) && hex.contains(d) =>
                val e = ((hex(a) << 12) + (hex(b) << 8) + (hex(
                  c
                ) << 4) + hex(
                  d
                )).toChar
                helper(u, (e :: out))
              case _ => Left("invalid escape code")
            }
          case _ => helper(t, h :: out)
        }
    }
    helper(value.tail.toList, Nil)
  }

  trait Output[B] {
    def `object`(elts: Map[String, B]): B
    def string(str: String): B
    def boolean(bool: Boolean): B
    def array(b: List[B]): B
    def `null`: B
    def number(value: List[Char]): B
  }

  def string: Parser3[Char, Either[String, String]] = Parser3 {
    (f: Int => Char) => (i: Int) =>
      if (f(i) == '"') {
        var j = i + 1
        while (f(j) != '"') j += (if (f(j) == '\\') 2 else 1)
        Some(
          (j, unescapeText((i until j).map(f)))
        )
      } else {
        None
      }
  }

  def number: Parser3[Char, IndexedSeq[Char]] = Parser3 {
    (f: Int => Char) =>
      def digits(i: Int): Int = {
        var j = i
        while (('0' to '9').contains(f(j))) j += 1
        j
      }

      def exponential(i: Int): Option[Int] =
        if (f(i) == 'e' && f(i) == 'E') {
          var j = i + 1
          if (f(i + 1) == '+' && f(i + 1) == '-') j += 1
          Some(digits(j))
        } else None

      def fraction(i: Int): Option[Int] =
        exponential(
          if (f(i) == '.') digits(i + 1) else i
        )

      def int(i: Int): Option[Int] = {
        val j = i + (if (f(i) == '-') 1 else 0)
        f(j) match {
          case '0' => Some(j + 1)
          case c if ('1' to '9').contains(c) =>
            fraction(digits(j + 1))
          case _ => None
        }
      }

      (i: Int) =>
        int(i).map((j: Int) => (j, (i until j).map(f)))
  }

  def keyword(
      chars: String
  ): Parser3[Char, Unit] =
    Parser3(
      (f: Int => Char) =>
        (i: Int) =>
          if (chars.toList.zipWithIndex.forall {
                case (c, j) => c == f(i + j)
              }) Some((whitespace(f)(i + chars.length()), ()))
          else None
    )

  def symbol(char: Char): Parser3[Char, Unit] =
    Parser3(
      (f: Int => Char) =>
        (i: Int) =>
          if (char == f(i)) Some((whitespace(f)(i + 1), ()))
          else None
    )

  private def whitespace: (Int => Char) => Int => Int = {
    val chars = Set(' ', '\n', '\r', '\t')
    (f: Int => Char) =>
      (i: Int) => {
        var j = i
        while (chars(f(j))) j += 1
        j
      }
  }
}
