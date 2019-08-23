package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import org.scalatest._
import nl.woupiestek.equalizer.parsing._
import scala.util.Try

class ParseTests extends FunSpec {

  object TestDef extends AST.Def[String] {
    def abstraction(name: String, body: String): String =
      name + "->" + body
    def application(
        operator: String,
        operand: String
    ): String = "(" + operator + ")" + operand
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

  val grammar: Grammar[String] =
    new Grammar(TestDef)

  def parse[X](
      parser: Parser[Char, String, X]
  )(
      input: String
  ): Option[Throwable] =
    Try(
      input.toList
        .foldLeft(parser)(_ derive _)
        .writes
    ).failed.toOption

  describe("parse") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.flatMap(
        parse(grammar.whitespace)
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
      val results = testStrings.flatMap(
        parse(grammar.identifier)
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3", "69", "0000000000")
      val results = testStrings.flatMap(
        parse(grammar.integer)
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
          "x -> x x"
        )
      val results =
        testStrings.flatMap(
          parse(grammar.defExp)
        )

      assert(
        results.isEmpty
      )
    }
  }

  lazy val grammar2: Grammar2[String] =
    new Grammar2(TestDef)

  def parse2[X](parser: Parser2[Char, String, X])(
      input: String
  ): Option[Throwable] =
    Try(
      input.toList
        .foldLeft(parser)(_ derive _)
        .writes
    ).failed.toOption

  describe("parse2") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.flatMap(
        parse2(grammar2.whitespace)
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
      val results = testStrings.flatMap(
        parse2(grammar2.onIdentifier(Parser2.point))
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3 ", "69", "0000000000")
      val results = testStrings.flatMap(
        parse2(grammar2.onIndex(Parser2.point))
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
          "x -> x x"
        )
      val results =
        testStrings.flatMap(
          parse2(grammar2.defExp)
        )

      assert(
        results.isEmpty
      )
    }
  }

  lazy val grammar3: Grammar3[String] =
    new Grammar3(TestDef)

  def parse3[X](
      parser: Parser3[Char, String, X]
  )(
      input: String
  ): Option[Throwable] =
    Try(
      input.toList
        .foldLeft(parser)(_ derive _)
        .writes
    ).failed.toOption

  describe("parse3") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.flatMap(
        parse3(grammar3.whitespace)
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
      val results = testStrings.flatMap(
        parse3(grammar3.identifier)
      )
      assert(results.isEmpty)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3", "69", "0000000000")
      val results = testStrings.flatMap(
        parse3(grammar3.integer)
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
          "x -> x x"
        )
      val results =
        testStrings.flatMap(
          parse3(grammar3.defExp)
        )

      assert(
        results.isEmpty
      )
    }
  }
}
