package nl.woupiestek.andrej.simple

import nl.woupiestek.andrej.simple.STerm._
import nl.woupiestek.andrej.simple.SType.hom
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm._
import org.scalatest.FunSpec

class STypeTest extends FunSpec {
  val s: Option[STerm] = \(\(\($(2) * $(0) * ($(1) * $(0)))))
  val k: Option[STerm] = \(\($(1)))

  describe("SType.getType") {

    it("types S combinator") {
      assert(s.nonEmpty)
      s.foreach(x => assert(x.getType === hom(hom(4, hom(5, 6)), hom(hom(4, 5), hom(4, 6)))))
    }

    it("types K combinator") {
      assert(k.nonEmpty)
      info(k.map(_.types).toString)
      k.foreach(x => assert(x.getType === hom(0, hom(1, 0))))
    }

    it("types I combinator") {
      val i = s * k * k
      info(i.map(_.types).toString)
      assert(i.nonEmpty)
      i.foreach(x => assert(x.getType === hom(15, 15)))
    }

    it("types I I") {
      val ii = \($(0)) * \($(0))
      assert(ii.nonEmpty)
      ii.foreach(_.getType === hom(2, 2))
    }

    val omegaC = \($(0) * $(0))

    it("types omega combinator") {
      assert(omegaC.isEmpty)
    }

    it("types Omega combinator") {
      assert((omegaC * omegaC).isEmpty)
    }

    val l = \(\($(1) * ($(0) * $(0))))

    it("types L combinator") {
      assert(l.isEmpty)
    }

    it("types Y combinator") {
      assert((l * l).isEmpty)
    }

  }

}