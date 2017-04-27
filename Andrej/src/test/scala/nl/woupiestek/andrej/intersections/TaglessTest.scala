package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._
import nl.woupiestek.andrej.intersections.Tagless._
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class TaglessTest extends FunSpec {

  describe("Tagless.typeOf") {
    it("types S combinator") {
      val s = \(\(\($(2) * $(0) * ($(1) * $(0)))))
      assert(typeOf(s) ===
        arrow(arrow(parameter(2), arrow(parameter(1), parameter(3))),
          arrow(arrow(parameter(0), parameter(1)),
            arrow(intersection(parameter(0), parameter(2)), parameter(3)))))
    }

    it("types K combinator") {
      val k = \(\($(1)))
      assert(typeOf(k) === arrow(parameter(0), arrow(omega, parameter(0))))
    }

    val omegaC = \($(0) * $(0))

    it("types omega combinator") {
      assert(typeOf(omegaC) === arrow(intersection(parameter(0), arrow(parameter(0), parameter(1))), parameter(1)))
    }

    it("types Omega combinator") {
      assert(typeOf(omegaC * omegaC) === omega)
    }

    val l = \(\($(1) * ($(0) * $(0))))

    it("types L combinator") {
      assert(typeOf(l) === arrow(arrow(parameter(1), parameter(2)),
        arrow(intersection(parameter(0), arrow(parameter(0), parameter(1))), parameter(2))))
    }

    it("types Y combinator") {
      assert(typeOf(l * l) === omega)
    }

  }

}
