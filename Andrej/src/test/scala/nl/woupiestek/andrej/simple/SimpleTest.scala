package nl.woupiestek.andrej.simple

import SimpleTyper._
import nl.woupiestek.andrej.simple.SimpleType.%
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class SimpleTest extends FunSpec {
  val s: Term = \(\(\($(2) * $(0) * ($(1) * $(0)))))
  val k: Term = \(\($(1)))

  describe("Tagless.typeOf") {

    it("types S combinator") {
      assert(typeOf(s) === Some((%(2) ->: %(1) ->: %(0)) ->: (%(2) ->: %(1)) ->: %(2) ->: %(0)))
    }

    it("types K combinator") {
      assert(typeOf(k) === Some(%(1) ->: %(2) ->: %(1)))
    }

    it("types I combinator") {
      assert(typeOf(s * k * k) === Some(%(1) ->: %(1)))
    }

    it("types I I") {
      assert(typeOf(\($(0)) * \($(0))) === Some(%(1) ->: %(1)))
    }

    val omegaC = \($(0) * $(0))

    it("types omega combinator") {
      assert(typeOf(omegaC) === None)
    }

    it("types Omega combinator") {
      assert(typeOf(omegaC * omegaC) === None)
    }

    val l = \(\($(1) * ($(0) * $(0))))

    it("types L combinator") {
      assert(typeOf(l) === None)
    }

    it("types Y combinator") {
      assert(typeOf(l * l) === None)
    }

  }

}
