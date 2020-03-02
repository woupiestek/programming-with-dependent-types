package nl.woupiestek.dtlang

//import zio._
import zio.test._
import zio.duration._
import zio.test.Assertion._
import zio.test.TestAspect._

object TyperTest
    extends DefaultRunnableSpec(
      suite("Y combinator")(
        test("parse the Y combinator") {
          val str = "f -> (x -> f(x x))(x -> f(x x))"
          val parser = Grammar.expression(Grammar.asString)
          val input = TestUtils.stringF(str)
          val Some((result, n)) = parser.run(input, 0)
          assert(n, equalTo(str.length)) &&
          assert(result(3), equalTo(str))
        } @@ timeout(1.minute),
        test("type the Y combinator") {
          val E =
            Grammar.asExpression
          val Y = E.abst(
            "f",
            E.appl(
              E.abst(
                "x",
                E.appl(
                  E.vari("f"),
                  E.appl(E.vari("x"), E.vari("x"))
                )
              ),
              E.abst(
                "x",
                E.appl(
                  E.vari("f"),
                  E.appl(E.vari("x"), E.vari("x"))
                )
              )
            )
          )
          val result = Y(0)
          assert(result._2.context.isEmpty, isTrue) &&
          assert(result._2.types.isEmpty, isFalse)
        } @@ timeout(1.minute)
      )
    )

object TestUtils {

  def stringF(str: String): Int => Char = {
    val dflt = (-1).toChar
    (i: Int) =>
      if (i >= 0 && i < str.length()) str.charAt(i)
      else dflt
  }
}
