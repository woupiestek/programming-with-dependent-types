package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType._
import nl.woupiestek.andrej.intersections.Tagless2._
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class Tagless2Test extends FunSpec {

  def arrow(x: InType, y: InType) = Arrow(Set(x), y)

  describe("Tagless2.typeOf") {
    it("types S combinator") {
      val s = \(\(\($(2) * $(0) * ($(1) * $(0)))))
      assert(typeOf(s) === Some(
        arrow(arrow(Parameter(2), arrow(Parameter(1), Parameter(3))),
          arrow(arrow(Parameter(0), Parameter(1)),
            Arrow(Set(Parameter(0), Parameter(2)), Parameter(3))))))
    }

    it("types K combinator") {
      val k = \(\($(1)))
      assert(typeOf(k) === Some(arrow(Parameter(0), Arrow(Set.empty, Parameter(0)))))
    }

    val omega = \($(0) * $(0))

    it("types omega combinator") {
      assert(typeOf(omega) === Some(Arrow(Set(Parameter(0), arrow(Parameter(0), Parameter(1))), Parameter(1))))
    }

    it("types Omega combinator") {
      assert(typeOf(omega * omega) === None)
    }

    val l = \(\($(1) * ($(0) * $(0))))

    it("types L combinator") {
      assert(typeOf(l) === Some(arrow(arrow(Parameter(1), Parameter(2)),
        Arrow(Set(Parameter(0), arrow(Parameter(0), Parameter(1))), Parameter(2)))))
    }

    val y = l * l

    it("types Y combinator") {
      assert(typeOf(y) === None)
    }

  }

}
