package nl.woupiestek.andrej.json

import nl.woupiestek.andrej.parser.Rule
import nl.woupiestek.andrej.parser.Rule.{ Fail, Point }
import org.apache.commons.lang3.StringEscapeUtils

// use http://seriot.ch/parsing_json.php for testing

class Grammar[J](j: Value[J]) {

  type R[E] = Rule[Option[Char], E]

  private val space = Rule.collect[Option[Char], Unit] { case Some(c) if Character.isWhitespace(c) => () }.zeroOrMore

  private def afterSpace[X](r: R[X]): R[X] = space.flatMap(_ => r)

  private val char = Rule.collect[Option[Char], Char] { case Some(c) => c }

  private def matchToken(a: Char): R[Unit] = for {
    _ <- Rule.collect[Option[Char], Unit] { case Some(c) if c == a => () }
    _ <- space
  } yield ()

  private def matchString(string: String): R[Unit] = Rule.matchList(string.map(Some(_)).toList)

  val jTrue: R[J] = matchString("true").map(_ => j.boolean(true))
  val jFalse: R[J] = matchString("false").map(_ => j.boolean(false))
  val jNull: R[J] = matchString("null").map(_ => j.jNull)

  //unicode presents a challenge
  def string: R[String] = {
    val escapes = Map('\\' -> '\\', '"' -> '"', '/' -> '/', 'b' -> '\b', 'n' -> '\n',
      'f' -> '\f', 'r' -> '\r', 't' -> '\t')
    val simpleEscape: R[Char] = Rule.collect[Option[Char], Char] { case Some(x) if escapes.contains(x) => escapes(x) }
    val hex: Map[Char, Int] = ((0 to 9).map(i => ('0' + i).toChar -> i) ++
      (0 to 6).flatMap(i => List(('a' + i).toChar -> (10 + i), ('A' + i).toChar -> (10 + i)))).toMap
    val hexDigit: R[Int] = Rule.collect[Option[Char], Int] { case Some(x) if hex.contains(x) => hex(x) }
    val unicodeEscape: R[Int] = for {
      'u' <- char
      h0 <- hexDigit
      h1 <- hexDigit
      h2 <- hexDigit
      h3 <- hexDigit
    } yield (h0 << 12) + (h1 << 8) + (h2 << 4) + h3

    def inhabited(builder: java.lang.StringBuilder): R[java.lang.StringBuilder] = char flatMap {
      case '"' => Point(builder)
      case '\\' => (simpleEscape.map(builder.append) | unicodeEscape.map(builder.appendCodePoint)).flatMap(inhabited)
      case other => inhabited(builder.append(other))
    }

    val b = new java.lang.StringBuilder
    for {
      '"' <- char
      builder <- matchToken('"').map(_ => b) | inhabited(b)
    } yield builder.toString
  }

  def number: R[J] = {
    val ncs: Map[Char, Int] = (0 to 9).map(i => ('0' + i).toChar -> i).toMap
    val digit = Rule.collect[Option[Char], Int] { case Some(c) if ncs.contains(c) => ncs(c) }
    for {
      sign <- Point(1) | Rule.collect[Option[Char], Int] { case Some('-') => -1 }
      n <- digit
      m <- if (n == 0) Point(0) else digit.zeroOrMore.map(_.foldLeft(n) { case (x, y) => 10 * x + y })
      p <- Point(0.0) | (for {
        _ <- Rule.collect[Option[Char], Unit] { case Some('.') => () }
        a <- digit.zeroOrMore.map(_.foldRight(0.0) { case (x, y) => 0.1 * (x + y) })
      } yield a)
      q <- Point(1.0) | (for {
        _ <- Rule.collect[Option[Char], Unit] { case Some(e) if e == 'e' || e == 'E' => () }
        s <- Point(1) | Rule.collect[Option[Char], Int] {
          case Some('+') => 1
          case Some('-') => -1
        }
        d <- digit.zeroOrMore.map(_.foldLeft(n) { case (x, y) => 10 * x + y })
      } yield math.pow(10.0, s * d))
      _ <- space
    } yield j.number(sign * (m + p) * q)
  }

  def array: R[J] = {
    def inhabited(elts: List[J]): R[List[J]] = for {
      elt <- value
      k <- char
      result <- k match {
        case ']' => afterSpace(Point(elt :: elts))
        case ',' => afterSpace(inhabited(elt :: elts))
        case _ => Fail
      }
    } yield result

    for {
      _ <- matchToken('[')
      elts <- matchToken(']').map(_ => Nil) | inhabited(Nil)
    } yield j.array(elts.reverse)
  }

  def jObject: R[J] = {
    def inhabited(pairs: Map[String, J]): R[Map[String, J]] = for {
      key <- string
      _ <- matchToken(':')
      value <- value
      next = pairs + (key -> value)
      c <- char
      result <- c match {
        case ',' => afterSpace(inhabited(next))
        case '}' => afterSpace(Point(next))
        case _ => Fail
      }
    } yield result

    val e = Map.empty[String, J]
    for {
      _ <- matchToken('{')
      pairs <- matchToken('}').map(_ => e) | inhabited(e)
    } yield j.jObject(pairs)
  }

  def value: R[J] = string.map(j.string) | number | jObject | array | jTrue | jFalse | jNull
}

trait Value[J] {
  def jNull: J

  def number(double: Double): J

  def string(string: String): J

  def boolean(boolean: Boolean): J

  def array(array: List[J]): J

  def jObject(obj: Map[String, J]): J
}

object PrettyPrint extends Value[String] {
  override def jNull: String = "null"

  override def number(double: Double): String = double.toString

  override def string(string: String): String = StringEscapeUtils.escapeJson(string)

  override def boolean(boolean: Boolean): String = boolean.toString

  override def array(array: List[String]): String = array.mkString("[", ", ", "]")

  override def jObject(obj: Map[String, String]): String = obj.map {
    case (key, value) => key + ": " + value
  }.mkString("{", ", ", "}")
}