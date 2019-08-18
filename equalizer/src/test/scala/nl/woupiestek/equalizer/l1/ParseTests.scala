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

  lazy val grammar: Grammar[String] =
    new Grammar(TestDef)

  def parse[X](
      parser: Parser[Char, String, X]
  )(
      input: String
  ): List[X] =
    input.toList
      .foldLeft(parser)(_ derive _)
      .writes

  describe("parse") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.filterNot(
        parse(grammar.whitespace)(_).isEmpty
      )
      assert(results == testStrings)
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
        parse(grammar.identifier)(_).isEmpty
      )
      assert(results == testStrings)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3", "69", "0000000000")
      val results = testStrings.filterNot(
        parse(grammar.integer)(_).isEmpty
      )
      assert(results == testStrings)
    }

    it("parses defs") {
      val testStrings =
        List(
          "x",
          //"much_longer_identifier",
          //"followed_by_white_space \t\r\n",
          "x y",
          "x = y; x",
          "x -> x",
          "x @ x",
          "x -> x x"
        )
      val results =
        testStrings.filterNot(
          str =>
            Try(parse(grammar.defExp)(str)).toOption
              .flatMap(_.headOption)
              .isEmpty
        )

      assert(
        results == testStrings
      )
    }
  }

  lazy val grammar2: Grammar2[String] =
    new Grammar2(TestDef)

  def parse2[X](
      parser: Parser2[Char, String, X]
  )(
      input: String
  ): List[X] =
    input.toList
      .foldLeft(parser)(_ derive _)
      .writes

  describe("parse2") {
    it("parses whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results = testStrings.filterNot(
        parse2(grammar2.whitespace)(_).isEmpty
      )
      assert(results == testStrings)
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
        parse2(grammar2.identifier)(_).isEmpty
      )
      assert(results == testStrings)
    }

    it("parses integers") {
      val testStrings =
        List("1234567890", "3", "69", "0000000000")
      val results = testStrings.filterNot(
        parse2(grammar2.integer)(_).isEmpty
      )
      assert(results == testStrings)
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
        testStrings.filterNot(
          str =>
            Try(parse2(grammar2.defExp)(str)).toOption
              .flatMap(_.headOption)
              .isEmpty
        )

      assert(
        results == testStrings
      )
    }
  }
}
