package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.LType._
import nl.woupiestek.andrej.intersections.Tagless._
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class TaglessTest extends FunSpec {
  val s: Term = \(\(\($(2) * $(0) * ($(1) * $(0)))))
  val k: Term = \(\($(1)))

  describe("Combinator") {
    val arrow1 = arrow(parameter(1), parameter(1))
    it("understands parameters") {
      val result = Combinator.combine(arrow(parameter(0), parameter(0)), List(arrow1))
      assert(result === arrow1)
    }

    it("binds variables") {
      val result = Combinator.leq(arrow1, Parameter(0))(Atomic(Parameter(0)))
      assert(result === arrow1)
    }

    //while the test fails, result is a correct type for the identity function.
    it("handles this just fine") {
      val result = Combinator.combine(typeOf(s), typeOf(k) :: typeOf(k) :: Nil)
      assert(result === arrow(parameter(0), parameter(0)))
    }

  }

  describe("Combinator2") {
    it("handles this just fine") {
      val result = Combinator2.combine(Combinator2.combine(typeOf(s), typeOf(k)), typeOf(k))
      assert(result === arrow(parameter(0), parameter(0)))
    }
  }

  describe("Tagless.typeOf") {

    it("types S combinator") {
      assert(typeOf(s) ===
        arrow(arrow(parameter(2), arrow(parameter(1), parameter(3))),
          arrow(arrow(parameter(0), parameter(1)),
            arrow(intersection(parameter(0), parameter(2)), parameter(3)))))
    }

    //stray parameter: one that does not occur in both variances

    it("types K combinator") {
      assert(typeOf(k) === arrow(parameter(0), arrow(omega, parameter(0))))
    }

    it("types I combinator") {
      assert(typeOf(s * k * k) === arrow(parameter(0), parameter(0)))
    }

    it("types I I") {
      assert(typeOf(\($(0)) * \($(0))) === arrow(parameter(0), parameter(0)))
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
