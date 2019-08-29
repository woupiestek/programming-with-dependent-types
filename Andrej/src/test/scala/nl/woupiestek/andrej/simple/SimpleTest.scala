package nl.woupiestek.andrej.simple

import SimpleTyper._
import nl.woupiestek.andrej.simple.SimpleType.%
import nl.woupiestek.andrej.simple.Solver.System
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class SimpleTest extends FunSpec {
  val s: Term = \(\(\($(2) * $(0) * ($(1) * $(0)))))
  val k: Term = \(\($(1)))

  info(s(Nil, %(0)).run(1).toString())
  info(k(Nil, %(0)).run(1).toString())

  describe("System") {
    val systemS = System(s(Nil, %(0)).run(1)._2).equated
    val skk = s * k * k
    val systemSKK = System(skk(Nil, %(0)).run(1)._2).equated
    info(systemS.toString)
    info(systemSKK.toString)

    it("assesses which variables need to be eliminated to get a solution") {
      assert(systemS.assess === Some(Set(5, 1, 2, 3, 8, 4)))
      assert(systemSKK.assess === Some(Set(0, 14, 1, 13, 2, 7, 3, 18, 11, 8, 15)))
    }

    it("eliminates any of those variables") {
      val no5 = systemS.eliminate(5)
      info(no5.toString)
      assert(no5.assess === Some(Set(1, 2, 3, 8, 4)))
    }

    it("solves by eliminiating all those variables") {
      val solution = systemS.solve
      info(solution.toString)
      assert(solution.map(_.assess) === Some(Some(Set.empty)))
      val solution2 = systemSKK.solve
      info(solution2.toString)
      assert(solution.map(_.assess) === Some(Some(Set.empty)))
    }

  }

  describe("SimpleTyper.typeOf") {

    it("types S combinator") {
      assert(typeOf(s) === Some(Set((%(9) ->: %(7) ->: %(6)) ->: (%(9) ->: %(7)) ->: %(9) ->: %(6))))
    }

    it("types K combinator") {
      assert(typeOf(k) === Some(Set(%(4) ->: %(3) ->: %(4))))
    }

    it("types I combinator") {
      assert(typeOf(s * k * k) === Some(Set(%(19) ->: %(19))))
    }

    it("types I I") {
      assert(typeOf(\($(0)) * \($(0))) === Some(Set(%(3) ->: %(3))))
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
