package nl.woupiestek.andrej.json

import nl.woupiestek.andrej.parser.Rule
import nl.woupiestek.andrej.parser.Rule.{ Point, collect, filter, matchList }
import org.apache.commons.lang3.StringEscapeUtils

// use http://seriot.ch/parsing_json.php for testing

class JsonGrammar[J](j: Value[J]) {

  type R[E] = Rule[Option[Char], E]

  private val jsonWhites = Set('\t', '\n', '\r', ' ')

  private val space: R[Unit] = filter[Option[Char]](_.exists(jsonWhites.contains)).zeroOrMore.map(_ => ())

  private val char = collect[Option[Char], Char] { case Some(c) => c }

  private def matchChar(c: Char) = filter[Option[Char]](_.contains(c))

  private def token(a: Char): R[Unit] = matchChar(a) ~ space

  private def matchString[X](string: String, x: => X): R[X] =
    matchList[Option[Char]](string.map(Some(_)).toList) ~ space.map(_ => x)

  val jTrue: R[J] = matchString("true", j.boolean(true))
  val jFalse: R[J] = matchString("false", j.boolean(false))
  val jNull: R[J] = matchString("null", j.jNull)

  //unicode presents a challenge
  def string: R[String] = {
    val escapes = Map('\\' -> '\\', '"' -> '"', '/' -> '/', 'b' -> '\b', 'n' -> '\n',
      'f' -> '\f', 'r' -> '\r', 't' -> '\t')
    val simpleEscape: R[Char] = collect[Option[Char], Char] { case Some(x) if escapes.contains(x) => escapes(x) }
    val hex: Map[Char, Int] = ((0 to 9).map(i => ('0' + i).toChar -> i) ++
      (0 to 6).flatMap(i => List(('a' + i).toChar -> (10 + i), ('A' + i).toChar -> (10 + i)))).toMap
    val hexDigit: R[Int] = collect[Option[Char], Int] { case Some(x) if hex.contains(x) => hex(x) }
    val unicodeEscape: R[Int] = for {
      _ <- matchChar('u')
      h0 <- hexDigit
      h1 <- hexDigit
      h2 <- hexDigit
      h3 <- hexDigit
    } yield (h0 << 12) + (h1 << 8) + (h2 << 4) + h3

    def inhabited(builder: java.lang.StringBuilder): R[java.lang.StringBuilder] = char.flatMap {
      case '"' => Point(builder)
      case '\\' => (simpleEscape.map(builder.append) | unicodeEscape.map(builder.appendCodePoint)).flatMap(inhabited)
      case other => inhabited(builder.append(other))
    }

    val b = new java.lang.StringBuilder
    for {
      _ <- matchChar('"')
      builder <- inhabited(b)
    } yield builder.toString
  }

  def number: R[J] = {
    val ncs: Map[Char, Int] = (0 to 9).map(i => ('0' + i).toChar -> i).toMap
    val digit = collect[Option[Char], Int] { case Some(c) if ncs.contains(c) => ncs(c) }

    val exponent: Rule[Option[Char], Int] = for {
      _ <- matchChar('e') | matchChar('E')
      s <- (matchChar('-') ~ Point(-1)) | (matchChar('+').zeroOrOne ~ Point(1))
      d <- digit.oneOrMore.map(_.foldLeft(0) { case (x, y) => 10 * x + y })
    } yield s * d

    val fraction: Rule[Option[Char], Double] = for {
      _ <- matchChar('.')
      a <- digit.oneOrMore.map(_.foldRight(0.0) { case (x, y) => 0.1 * (x + y) })
    } yield a

    val natural: Rule[Option[Char], Int] = for {
      n <- digit
      m <- if (n == 0) Point(0) else digit.zeroOrMore.map(_.foldLeft(n) { case (x, y) => 10 * x + y })
    } yield m

    val positive: Rule[Option[Char], Double] = for {
      m <- natural
      p <- fraction.zeroOrOne.map(_.getOrElse(0.0))
      q <- exponent.zeroOrOne.map(_.getOrElse(0))
      _ <- space
    } yield (m + p) * math.pow(10.0, q)

    positive.map(j.number) | (matchChar('-') ~ positive.map(d => j.number(d * -1)))
  }

  def array: R[J] = {
    def inhabited(elts: List[J]): R[List[J]] = for {
      elt <- value
      result <- (token(']') ~ Point(elt :: elts)) | (token(',') ~ inhabited(elt :: elts))
    } yield result

    for {
      _ <- token('[')
      elts <- token(']').map(_ => Nil) | inhabited(Nil)
    } yield j.array(elts.reverse)
  }

  def jObject: R[J] = {
    def inhabited(pairs: Map[String, J]): R[Map[String, J]] = for {
      key <- string
      _ <- token(':')
      value <- value
      next = pairs + (key -> value)
      result <- (token(',') ~ inhabited(next)) | (token('}') ~ Point(next))
    } yield result

    val e = Map.empty[String, J]
    for {
      _ <- token('{')
      pairs <- token('}').map(_ => e) | inhabited(e)
    } yield j.jObject(pairs)
  }

  def value: R[J] = jTrue | jFalse | jNull | array | jObject | number | string.map(j.string)
}

trait Value[J] {
  def jNull: J

  def number(double: Double): J

  def string(string: String): J

  def boolean(boolean: Boolean): J

  def array(array: List[J]): J

  def jObject(obj: Map[String, J]): J
}

object PrintJson extends Value[String] {
  override def jNull: String = "null"

  override def number(double: Double): String = double.toString

  override def string(string: String): String = "\"" + StringEscapeUtils.escapeJson(string) + "\""

  override def boolean(boolean: Boolean): String = boolean.toString

  override def array(array: List[String]): String = array.mkString("[", ", ", "]")

  override def jObject(obj: Map[String, String]): String = obj.map {
    case (key, value) => string(key) + ": " + value
  }.mkString("{", ", ", "}")
}