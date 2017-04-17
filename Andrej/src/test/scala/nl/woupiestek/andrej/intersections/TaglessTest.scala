package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InType._
import nl.woupiestek.andrej.intersections.Tagless._
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class TaglessTest extends FunSpec {

  describe("Tagless.typeOf") {
    it("types S combinator") {
      val s = \(\(\($(2) * $(0) * ($(1) * $(0)))))
      assert(typeOf(s) === forall(4,
        arrow(arrow(Var(2), arrow(Var(1), Var(3))),
          arrow(arrow(Var(0), Var(1)),
            arrow(intersection(Set(Var(0), Var(2))), Var(3))))))
    }

    it("types K combinator") {
      val k = \(\($(1)))
      assert(typeOf(k) === forall(1, arrow(Var(0), arrow(top, Var(0)))))
    }

    val omega = \($(0) * $(0))

    it("types omega combinator") {
      assert(typeOf(omega) === forall(2, arrow(intersection(Set(Var(0), arrow(Var(0), Var(1)))), Var(1))))
    }

    it("types Omega combinator") {
      assert(typeOf(omega * omega) === forall(2, top))
    }
  }

}
