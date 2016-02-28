package nl.woupiestek.dtlang

object Intersector {

  sealed trait Sort

  case class Of(term: Term) extends Sort

  case class Arrow(dom: Set[Arrow], cod: Int) extends Sort

  sealed trait Term {
    def getSorts: Set[Sort]
  }

  case class Vari(index: Int) extends Term {
    override def getSorts: Set[Sort] = ???
  }

  case class Appl(operator: Term, operand: Term) extends Term {
    override def getSorts: Set[Sort] = ???
  }

  case class Abst(body: Term) extends Term {
    override def getSorts: Set[Sort] = ???
  }

}
