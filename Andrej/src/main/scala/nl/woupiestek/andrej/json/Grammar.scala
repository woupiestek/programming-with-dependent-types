package nl.woupiestek.andrej.json

import nl.woupiestek.andrej.parser.Rule
import nl.woupiestek.andrej.parser.Rule.{ Fail, Point }

class Grammar[J](j: JSON[J]) {

  type R[E] = Rule[Option[Char], E]

  private lazy val space = Rule.collect[Option[Char], Unit] { case Some(c) if Character.isWhitespace(c) => () }.zeroOrMore

  private val char = Rule.collect[Option[Char], Char] { case Some(c) => c }

  private val keyToken = for {
    c <- char
    _ <- space
  } yield c

  def term: R[J] = ???

  private def matchString(string: String): R[Unit] = Rule.matchList(string.map(Some(_)).toList)

  val jTrue: R[J] = matchString("true").map(_ => j.boolean(true))
  val jFalse: R[J] = matchString("false").map(_ => j.boolean(false))
  val jNull: R[J] = matchString("null").map(_ => j.jNull)

  //unicode presents a challenge
  val string: R[String] = {
    val empty = char collect { case '"' => Nil }

    val simpleEscape: R[Char] = char collect {
      case '\\' => '\\'
      case '"' => '"'
      case '/' => '/'
      case 'b' => '\b'
      case 'n' => '\n'
      case 'f' => '\f'
      case 'r' => '\r'
      case 't' => '\t'
    }

    val hex = "0123456789abcdefABCDEF".toSet
    val unicodeEscape: R[Char] = for {
      'u' <- char
      h0 <- char.filter(hex.contains)
      h1 <- char.filter(hex.contains)
      h2 <- char.filter(hex.contains)
      h3 <- char.filter(hex.contains)
    } yield ???

    def inhabited(stack: List[Char]): R[List[Char]] =
      char flatMap {
        case '"' => Point(stack)
        case '\\' => for {
          second <- simpleEscape | unicodeEscape
          y <- inhabited(second :: stack)
        } yield y
        case other => inhabited(other :: stack)
      }

    for {
      '"' <- char
      stack <- empty | inhabited(Nil)
    } yield stack.reverse.mkString
  }

  def list: R[J] = {
    val empty: R[List[J]] = for {
      ']' <- keyToken
    } yield Nil

    def inhabited(elts: List[J]): R[List[J]] = for {
      elt <- term
      k <- keyToken
      next = elt :: elts
      result <- k match {
        case ']' => Point(next)
        case ',' => inhabited(next)
        case _ => Fail
      }
    } yield result

    for {
      '[' <- keyToken
      elts <- empty | inhabited(Nil)
    } yield j.array(elts)
  }

  def objekt: R[J] = {
    val empty = for {
      '}' <- keyToken
    } yield Map.empty[String, J]

    def inhabited(pairs: Map[String, J]): R[Map[String, J]] = for {
      key <- string
      ':' <- keyToken
      value <- term
      next = pairs + (key -> value)
      c <- keyToken
      result <- c match {
        case ',' => inhabited(next)
        case '}' => Point(next)
        case _ => Fail
      }
    } yield result

    for {
      '{' <- keyToken
      pairs <- empty | inhabited(Map.empty)
    } yield j.jObject(pairs)
  }

}

trait JSON[J] {
  def jNull: J

  def number(double: Double): J

  def string(string: String): J

  def boolean(boolean: Boolean): J

  def array(array: List[J]): J

  def jObject(obj: Map[String, J]): J
}
