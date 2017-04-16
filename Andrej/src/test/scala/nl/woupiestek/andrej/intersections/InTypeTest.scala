package nl.woupiestek.andrej.intersections

import nl.woupiestek.andrej.intersections.InTerm._
import nl.woupiestek.andrej.intersections.InType.{ Var, arrow, intersection, top }
import org.scalatest.FunSpec

class InTypeTest extends FunSpec {

  describe("typed") {
    it("types S combinator") {
      val s = Abstraction(Abstraction(Abstraction(Application(Application(Variable(2), Variable(0)), Application(Variable(1), Variable(0))))))
      val (typ, state) = InTerm.typed(s, Nil, Nil).go(start)
      assert(state.arity === 4)
      assert(typ === arrow(arrow(Var(2), arrow(Var(1), Var(3))), arrow(
        arrow(Var(0), Var(1)),
        arrow(intersection(Set(Var(0), Var(2))), Var(3)))))
    }

    it("types K combinator") {
      val k = Abstraction(Abstraction(Variable(1)))
      val (typ, state) = InTerm.typed(k, Nil, Nil).go(start)
      assert(state.arity === 1)
      assert(typ === arrow(Var(0), arrow(top, Var(0))))
    }

    val omega = Abstraction(Application(Variable(0), Variable(0)))
    
    it("types omega combinator") {
      val (typ, state) = InTerm.typed(omega, Nil, Nil).go(start)
      assert(state.arity === 2)
      assert(typ === arrow(intersection(Set(Var(0), arrow(Var(0), Var(1)))), Var(1)))
    }

    it("types Omega combinator") {
      val (typ, state) = InTerm.typed(Application(omega, omega), Nil, Nil).go(start)
      assert(state.arity === 2)
      assert(typ === top)
    }
  }

}
