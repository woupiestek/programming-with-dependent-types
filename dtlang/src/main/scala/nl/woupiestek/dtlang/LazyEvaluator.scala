package nl.woupiestek.dtlang

object LazyEvaluator {

  sealed trait Expr {
    def replace(index: Int, e: Expr): Expr

    def value: Option[Expr]

    def getType(context: List[Expr]): Option[Expr]

    def meets(other: Expr): Boolean
  }

  case class Vari(index: Int) extends Expr {
    override def replace(j: Int, e: Expr): Expr =
      if (j > index) this
      else if (j == index) e
      else Vari(index - 1)

    override def getType(context: List[Expr]): Option[Expr] = context lift index

    override val value: Option[Expr] = Some(this)

    override def meets(other: Expr): Boolean = other.value.contains(this)
  }

  case class Appl(operator: Expr, operand: Expr) extends Expr {
    override def replace(i: Int, e: Expr): Expr = Appl(operator.replace(i, e), operand.replace(i, e))

    override def getType(context: List[Expr]): Option[Expr] = for {
      Prod(a, b) <- operator.getType(context)
      c <- operand.getType(context) if a meets c
    } yield b.replace(0, operand)

    override lazy val value: Option[Expr] = for {
      Abst(a, b) <- operator.value
      c <- b.replace(0, operand).value
    } yield c

    override def meets(other: Expr): Boolean = this.value.exists {
      case Appl(a, b) => other.value.exists {
        case Appl(c, d) => (a meets c) && (b meets d)
      }
      case x => x meets other
    }
  }

  case class Abst(dom: Expr, body: Expr) extends Expr {
    override def replace(index: Int, e: Expr): Expr =
      Abst(dom.replace(index, e), body.replace(index + 1, e))

    override def getType(context: List[Expr]): Option[Expr] =
      for (t <- body.getType(dom :: context)) yield Prod(dom, t)

    override val value: Option[Expr] = Some(this)

    override def meets(other: Expr): Boolean = other.value.exists {
      case Abst(d, b) => (d meets dom) && (b meets body)
    }
  }

  case class Prod(dom: Expr, body: Expr) extends Expr {
    override def replace(index: Int, e: Expr): Expr =
      Prod(dom.replace(index, e), body.replace(index + 1, e))

    override def getType(context: List[Expr]): Option[Expr] = Some(Univ)

    override val value: Option[Expr] = Some(this)

    override def meets(other: Expr): Boolean = other.value.exists {
      case Prod(d, b) => (d meets dom) && (b meets body)
    }
  }

  case object Univ extends Expr {
    override def replace(index: Int, e: Expr): Expr = this

    override def getType(context: List[Expr]): Option[Expr] = None

    override val value: Option[Expr] = Some(this)

    override def meets(other: Expr): Boolean = other.value.contains(this)
  }

}
