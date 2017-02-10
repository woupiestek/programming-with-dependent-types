package nl.woupiestek.andrej

import nl.woupiestek.andrej.json.{ JsonGrammar, PrintJson }
import nl.woupiestek.andrej.parser.{ Rule, StringParser }
import org.scalatest.FunSpec

class JsonTest extends FunSpec {

  val grammar = new JsonGrammar(PrintJson)
  val rule: Rule[Option[Char], String] = grammar.value

  describe("the json grammar") {

    it("fail on certain strings") {
      val tests = List("{\"a\":", "{\"\":", "[\"\\", "[-2.]", "[1.]", "[0.e1]", "[2.e+3]", "[2.e-3]", "[2.e3]", "[\"\\a\"]", "\\UA66D")
      val errors = tests.filter(s => StringParser.parse[String](rule, s).isRight)
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

    it("fail on certain strings 2") {
      val tests = List("[\\uD800\\uD800\\x]", "[0.3e]", "[1.0e]", "[0.3e+]", "[0E+]", "0E", "0e+", "0e", "1.0e+", "1.0e-", "9.e+")
      val errors = tests.map(s => (s, StringParser.parse[String](rule, s))).collect { case (x, Right(y)) => x + " <=> " + y }
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

    it("fail on certain strings 3") {
      val tests = List("\"\",", "1,", "\"id\":0,", "-", "{\"a\":\"b\"}/**/", "", "{\"a\":\"b\"}#", "[1]x", "[][]")
      val errors = tests.map(s => (s, StringParser.parse[String](rule, s))).collect { case (x, Right(y)) => x + " <=> " + y }
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

    it("fail on certain strings 4") {
      val tests = List("{\"a\": true} \"x\"", "\"\"x", "[-01]", "[-012]", "[012]", "[-.123]", "2@", "[\"x\"]]", "[1]]")
      val errors = tests.map(s => (s, StringParser.parse[String](rule, s))).collect { case (x, Right(y)) => x + " <=> " + y }
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

    it("fail on certain strings 5") {
      val tests = List("{}}", "{\"a\":\"b\"}#{}", "1]", "{'a':0}", "['single quote']", "[NaN]", "[Infinity]")
      val errors = tests.map(s => (s, StringParser.parse[String](rule, s))).collect { case (x, Right(y)) => x + " <=> " + y }
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

    it("fail on certain strings 6") {
      val tests = List("[-Infinity]", "[True]", "[-1.0.]", "[0.1.2]", "[1eE2]", "[1+2]", "[0e+-1]", "{\"a\":/*comment*/\"b\"}")
      val errors = tests.map(s => (s, StringParser.parse[String](rule, s))).collect { case (x, Right(y)) => x + " <=> " + y }
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

    it("succeed on certain strings") {
      val tests = List("null", "false", "42", "-0.1", "true", "\" \"", "\"asd\"", "\"\"")
      val errors = tests.filter(s => StringParser.parse[String](rule, s).isLeft)
      info(errors.mkString(" "))
      assert(errors.isEmpty)
    }

  }

}
