package nl.woupiestek.equalizer.l1

import scalaz._
import Scalaz._
import org.scalatest._
import nl.woupiestek.equalizer.parsing._

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

  lazy val grammar: Grammar[Fap, String, String] =
    new Grammar(TestType, TestDef)

  def parseDef(input: String): List[String] = {
    input.toList
      .foldLeft(grammar.defExp)(_ derive _)
      .matches
      .toList
  }

  describe("parseDef") {
    it("parses defs") {
      try {
        val m = parseDef("x -> x x")
        assert(m.nonEmpty)
        assert(m.head == "x -> x x")
      } catch {
        case e: Exception =>
          e.printStackTrace()
          fail(String.valueOf(e))
      }
    }
  }

}
