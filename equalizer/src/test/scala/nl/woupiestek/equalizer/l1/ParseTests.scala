package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import org.scalatest._
import nl.woupiestek.equalizer.parsing._
import nl.woupiestek.equalizer.simpler.X

class ParseTests extends FunSpec {

  object TestType extends AST.Type[String] {
    def product(types: List[String]): String =
      types.mkString("<", ",", ">")
    def arrow(source: String, target: String): String =
      "(" + source + ")->" + target
    def fix(name: String, context: String): String =
      name + "@" + context
    def let(
        name: String,
        value: String,
        context: String
    ): String =
      s"$name=$value;$context"
    def variable(name: String): String = name

  }
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

  lazy val grammar: Grammar[Fmp, String, String] =
    new Grammar(TestType, TestDef)

  def parse[X](
      parser: ParserT[Fmp, Char, String, X]
  )(
      input: String
  ): List[X] =
    input.toList
      .foldLeft(parser)(_ derive _)
      .writes
      .toList

  describe("parseString") {
    it("parser whitespace") {
      val testStrings = List(" ", "\n", "\r", "\t", "  ")
      val results =
        testStrings.map(parse(grammar.whitespace))

      assert(
        results
          .filter(_.nonEmpty)
          .length == testStrings.length
      )
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
        testStrings.map(parse(grammar.defExp))

      assert(
        results
          .filter(_.nonEmpty)
          .map(_.head) == testStrings
      )
    }
  }

}
