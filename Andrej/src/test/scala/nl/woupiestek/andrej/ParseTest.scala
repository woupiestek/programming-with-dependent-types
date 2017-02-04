package nl.woupiestek.andrej

import nl.woupiestek.andrej.Lamb._
import nl.woupiestek.andrej.parser.{ DTLGrammar, PrettyPrint, Rule, StringParser }
import org.scalatest._

class ParseTest extends FunSuite {

  val rule: Rule[Option[Char], Option[Lamb]] =
    new DTLGrammar(new StripVars(Lamb.Instance)).term.map(x => x(Nil))

  test("should parse type") {
    assert(StringParser.parse(rule, "type").contains(Univ))
    assert(StringParser.parse(rule, "\\t:type.\\x:t.x").contains(Abst(Univ, Abst(Vari(0), Vari(0)))))
    assert(StringParser.parse(rule, "pi t:type.t").contains(Prod(Univ, Vari(0))))
  }

  test("parsing pretty print") {
    val s =
      Abst(Univ,
        Abst(Univ,
          Abst(Univ,
            Abst(Prod(Vari(2), Prod(Vari(2), Vari(2))),
              Abst(Prod(Vari(3), Vari(2)),
                Abst(Prod(Vari(4), Vari(2)),
                  Appl(Vari(2), Vari(0) :: Appl(Vari(1), Vari(0) :: Nil) :: Nil)))))))

    val string = Lamb.fold(s, PrettyPrint)(Nil)
    assert(string === "(\\x0:type.(\\x1:type.(\\x2:type.(\\x3:(pi x3:x0.(pi x4:x1.x2)).(\\x4:(pi x4:x0.x2).(\\x5:(pi x5:x0.x3).((x3 x5) (x4 x5))))))))")
    assert(StringParser.parse(rule, "").contains(s))
  }

}

