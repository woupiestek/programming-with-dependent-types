package nl.woupiestek.andrej.test

import scala.util.Success
import org.scalatest._
import util.parsing.combinator._
import nl.woupiestek.andrej.core._
import nl.woupiestek.andrej.parse._

class ParseTest extends FlatSpec {

  def check(parser: Parse.Parser[Expression], input: String, output: Expression) {
    assert(Parse.parseAll(parser, input).get == output)
  }

  "The parser" should "parse variables" in check(Parse.variable, "x", Named("x"))
  it should "recognize universes" in check(Parse.universe, "Type 666", Universe(666))
  it should "recognize abstractions" in check(Parse.abstraction, "fun x:T=>M", Abstraction(Named("x"), Named("T"), Named("M")))
  it should "recognize dependent products" in check(Parse.pi, "forall x:T=>M", Pi(Abstraction(Named("x"), Named("T"), Named("M"))))
  it should "recognize cuts" in check(Parse.cut, "[x=N]M", Cut(Named("x"), Named("N"), Named("M")))
  it should "recognize applications" in check(Parse.application, "f x", Application(Named("f"), Named("x")))

  def checkParboil(input: String, output: Expression) {
    assert(Parboil(input) == Success(output))
  }

  "Parboil" should "recognize variables" in checkParboil("x", Named("x"))
  it should "recognize universes" in checkParboil("type 666", Universe(666))
  it should "recognize abstraction" in checkParboil("(fun x:y=>z)", Abstraction(Named("x"), Named("y"), Named("z")))
  it should "recognize products" in checkParboil("(forall x:y=>z)", Pi(Abstraction(Named("x"), Named("y"), Named("z"))))
  it should "recognize cuts" in checkParboil("(let x=y.z)", Cut(Named("x"), Named("y"), Named("z")))
  it should "recognize applications" in checkParboil("(x y)", Application(Named("x"), Named("y")))


  def checkLex(input: String, output: TokenTree) {
    assert(Lex(input) == Success(output))
  }

  "Lex" should "recognize tokens" in checkLex("x", Leaf("x"))
  //FIXME: it should "recognize parens" in checkLex("(x y)", Fork(List(Leaf("x"), Leaf("y"))))

  def checkParse2(input: String, output: Expression) = {
    Lex(input).map { lexed =>
      assert(Parse2(lexed) == Some(output))
    }
  }

  "Parse2" should "recognize variables" in checkParse2("x", Named("x"))
  it should "recognize universes" in checkParse2("(type 666)", Universe(666))
  it should "recognize abstraction" in checkParse2("(function x y z)", Abstraction(Named("x"), Named("y"), Named("z")))
  it should "recognize products" in checkParse2("(product x y z)", Pi(Abstraction(Named("x"), Named("y"), Named("z"))))
  it should "recognize cuts" in checkParse2("(let x y z)", Cut(Named("x"), Named("y"), Named("z")))
  it should "recognize applications" in checkParse2("(x y)", Application(Named("x"), Named("y")))

}


