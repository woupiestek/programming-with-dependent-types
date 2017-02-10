package nl.woupiestek.andrej

import nl.woupiestek.andrej.Lamb.Instance._
import nl.woupiestek.andrej.parser.{ DTLGrammar, PrettyPrint, Rule, StringParser }
import org.scalatest._

class ParseTest extends FunSpec {
  def rule(vars: List[String]): Rule[Option[Char], Option[Lamb]] =
    new DTLGrammar(new StripVars(Lamb.Instance)).term.map(x => x(vars))
  val xyz = List("c", "b", "a")

  describe("parsing") {

    it("should recognize the type of types") {
      assert(StringParser.parse(rule(Nil), "type") === Right(Some(omega)))
    }
    it("should recognize the family of identities") {
      assert(StringParser.parse(rule(Nil), "\\t:type.\\x:t.x") === Right(Some(lambda(omega, lambda(get(0), get(0))))))
    }
    it("should recognize the initial type") {
      val i = pi(omega, get(0))
      val j = Lamb.fold(i, PrettyPrint)(Nil)
      info(j)
      assert(StringParser.parse(rule(Nil), j) === Right(Some(i)))
    }

    it("should produce none on missing variables") {
      assert(StringParser.parse(rule(Nil), "pi t:type.s") === Right(None))
    }

    it("should fail on gibberish 2") {
      assert(StringParser.parse(rule(xyz), "(type % b c)") === Left(6))
    }
  }

  describe("parsing & printing") {
    val appl = application(application(get(2), get(0)), application(get(1), get(0)))
    val abs1 = lambda(pi(get(2), pi(get(2), get(2))), lambda(pi(get(3), get(2)), lambda(pi(get(4), get(2)), appl)))
    val abs2 = lambda(omega, lambda(omega, lambda(omega, abs1)))

    it("should handle applications") {
      val test1 = Lamb.fold(appl, PrettyPrint)(xyz)
      info(test1)
      assert(StringParser.parse(rule(xyz), test1) === Right(Some(appl)))
    }

    it("should handle simple abstractions") {
      val test2 = Lamb.fold(abs1, PrettyPrint)(xyz)
      info(test2)
      assert(StringParser.parse(rule(xyz), test2) === Right(Some(abs1)))
    }

    it("should handle type abstractions") {
      val test3 = Lamb.fold(abs2, PrettyPrint)(Nil)
      info(test3)
      assert(StringParser.parse(rule(Nil), test3) === Right(Some(abs2)))
    }

    it("should handle let bindings") {
      val lets = Lamb.Instance.push(omega, get(0))
      val test4 = Lamb.fold(lets, PrettyPrint)(xyz)
      info(test4)
      assert(StringParser.parse(rule(xyz), test4) === Right(Some(lets)))
    }
  }

}

