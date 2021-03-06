package nl.woupiestek.equalizer.l1

import org.scalatest._
import nl.woupiestek.equalizer.parsing._

class ParseTests extends FunSpec {
  object TestDef extends AST.Def[String] {
    def abstraction(name: String, body: String): String =
      "(" + name + "->" + body + ")"
    def application(
        operator: String,
        operand: String
    ): String = "(" + operator + " " + operand + ")"
    def fix(name: String, body: String): String =
      name + "@" + body
    def let(
        name: String,
        value: String,
        context: String
    ): String = s"$name=$value;$context"
    def project(d: String, index: Int): String =
      d + "_" + index
    def tuple(defs: List[String]): String =
      defs.mkString("<", ",", ">")
    def unfold(body: String): String = body + "'"
    def variable(name: String): String = name
  }

  def timed[X](x: => X): X = {
    val t = System.nanoTime()
    val y = x
    info((System.nanoTime() - t).toString() + " nanos")
    y
  }

  def parse[X](
      parser: Parser[Char, String, X]
  ): String => Boolean =
    input =>
      timed(
        Parser
          .parse(parser, function(input), 0)
      ).flatMap(_._2.toOption)
        .map(x => info(String.valueOf(x)))
        .nonEmpty

  //parse3(Parser.parser3(parser))

  describe("parse") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.filterNot(
        parse(Grammar.whitespace)
      )
      assert(results.isEmpty)
    }

    it("parses identifiers") {
      val testStrings = List(
        "a",
        "Ab",
        "_c",
        "d56",
        "e  ",
        "x",
        "much_longer_identifier",
        "followed_by_white_space \t\r\n"
      )
      val results = testStrings.filterNot(
        parse(Grammar.identifier)
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3", "69", "0000000000")
      val results = testStrings.filterNot(
        parse(Grammar.integer)
      )
      assert(results.isEmpty)
    }

    it("parses defs") {
      val testStrings =
        List(
          "x",
          "much_longer_identifier",
          "followed_by_white_space \t\r\n",
          "x y",
          "x = y; x",
          "x -> x",
          "x @ x",
          "x -> x x",
          "f -> (x -> f(x x))(x -> f(x x))"
        )
      val results =
        testStrings.filterNot(
          parse(Grammar.expression(TestDef))
        )

      assert(
        results.isEmpty
      )
    }
  }

  def parse2[X](
      parser: Parser2[Char, String, X]
  ): String => Boolean =
    parse3(Parser2.parser3(parser))

  describe("parse2") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.filterNot(
        parse2(Grammar2.whitespace)
      )
      assert(results.isEmpty)
    }

    it("parses identifiers") {
      val testStrings = List(
        "a",
        "Ab",
        "_c",
        "d56",
        "e  ",
        "x",
        "much_longer_identifier",
        "followed_by_white_space \t\r\n"
      )
      val results = testStrings.filterNot(
        parse2(Grammar2.onIdentifier(Parser2.point))
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3 ", "69", "0000000000")
      val results = testStrings.filterNot(
        parse2(Grammar2.onIndex(Parser2.point))
      )
      assert(results.isEmpty)
    }

    it("parses defs") {
      val testStrings = //failed
        List(
          "x",
          "much_longer_identifier",
          "followed_by_white_space \t\r\n",
          "x y",
          "x = y; x",
          "x -> x",
          "x @ x",
          "x -> x x",
          "f -> (x -> f(x x))(x -> f(x x))"
        )
      val results =
        testStrings.filterNot(
          parse2(Grammar2.defExp(TestDef))
        )

      assert(
        results.isEmpty
      )
    }
  }

  def parse3[X](
      parser: => Parser3[Char, X]
  )(
      input: String
  ): Boolean =
    timed(
      parser
        .run(function(input))(0)
    ).map(x => info(String.valueOf(x)))
      .nonEmpty

  describe("parse3") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.filterNot(
        parse3(
          Parser3.unit(
            (f: Int => Char) =>
              (i: Int) => Some(Grammar3.whitespace(i)(f))
          )
        )
      )
      assert(results.isEmpty)
    }

    it("parses identifiers") {
      val testStrings = List(
        "a",
        "Ab",
        "_c",
        "d56",
        "e  ",
        "x",
        "much_longer_identifier",
        "followed_by_white_space \t\r\n"
      )
      val results = testStrings.filterNot(
        parse3(Grammar3.identifier)
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1244567890", "4 ", "69", "0000000000")
      val results = testStrings.filterNot(
        parse3(Grammar3.integer)
      )
      assert(results.isEmpty)
    }

    it("parses defs") {
      val testStrings =
        List(
          "x",
          "much_longer_identifier",
          "followed_by_white_space \t\r\n",
          "x y",
          "x = y; x",
          "x -> x",
          "x @ x",
          "x -> x x",
          "f -> (x -> f(x x))(x -> f(x x))"
        )
      val results =
        testStrings.filterNot(
          parse3(Grammar3.expression(TestDef))
        )

      assert(
        results.isEmpty
      )
    }
  }

  // 4

  def parse4[X](
      parser: => Parser4[Char, X]
  )(
      input: String
  ): Boolean =
    try {
      timed(
        parser
          .run(Input.fromString(input))
      ).map { case (_, x) => info(String.valueOf(x)) }.nonEmpty
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        false
    }

  describe("parse4") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.filterNot(
        parse4(Grammar4.whitespace)
      )
      assert(results.isEmpty)
    }

    it("parses identifiers") {
      val testStrings = List(
        "a",
        "Ab",
        "_c",
        "d56",
        "e  ",
        "x",
        "much_longer_identifier",
        "followed_by_white_space \t\r\n"
      )
      val results = testStrings.filterNot(
        parse4(Grammar4.identifier)
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1244567890", "4", "69", "0000000000")
      val results = testStrings.filterNot(
        parse4(Grammar4.integer)
      )
      assert(results.isEmpty)
    }

    it("parses defs") {
      val testStrings =
        List(
          "x",
          "much_longer_identifier",
          "followed_by_white_space \t\r\n",
          "x y",
          "x = y; x",
          "x -> x",
          "x @ x",
          "x -> x x",
          "f -> (x -> f(x x))(x -> f(x x))"
        )
      val results =
        testStrings.filterNot(
          parse4(Grammar4.defExp(TestDef))
        )

      assert(
        results.isEmpty
      )
    }
  }

  def function(input: String) =
    (i: Int) =>
      if (i >= 0 && i < input.length) input.charAt(i)
      else (-1).toChar
}
