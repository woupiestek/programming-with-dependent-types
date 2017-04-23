package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InTerm._
import nl.woupiestek.andrej.intersections.InType._
import org.scalatest.FunSpec

class InTypeTest extends FunSpec {

  describe("InTerm.typed") {
    it("types S combinator") {
      val s = Abstraction(Abstraction(Abstraction(Application(Application(Variable(2), Variable(0)), Application(Variable(1), Variable(0))))))
      assert(typeOf(s) ===
        arrow(arrow(Var(2), arrow(Var(1), Var(3))),
          arrow(arrow(Var(0), Var(1)),
            arrow(intersection(Set(Var(0), Var(2))), Var(3)))))
    }

    it("types K combinator") {
      val k = Abstraction(Abstraction(Variable(1)))
      assert(typeOf(k) === arrow(Var(0), arrow(top, Var(0))))
    }

    val omega = Abstraction(Application(Variable(0), Variable(0)))

    it("types omega combinator") {
      assert(typeOf(omega) === arrow(intersection(Set(Var(0), arrow(Var(0), Var(1)))), Var(1)))
    }

    it("types Omega combinator") {
      assert(typeOf(Application(omega, omega)) === top)
    }
  }

}
