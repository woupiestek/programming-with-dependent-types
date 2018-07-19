package nl.woupiestek.andrej.simple

import nl.woupiestek.andrej.simple.SType.{ Arrow, Parameter }
import nl.woupiestek.andrej.typeclasses.UntypedLambdaTerm

import scala.annotation.tailrec

/*determine
 *  a) size of the term
 *  b) coordinates of free variables, but relative to input as in the list of inputs
 *  c) we might even try to build up the types already.
 *
 *  Important: keep track of the ordering. Keep assuming that the whole term comes last!
 *
 *  If a variable is untyped, there is no subterm to point to. For this case I needed to alter the structure.
 */

sealed trait SType {
  def parameters: Set[Int]

  def replace(i: Int, x: SType): SType

  def shift(m: Int): SType

  override def toString: String = SType.string(this, Nil)
}

object SType {

  def hom(source: SType, target: SType): SType = Arrow(source, target)

  def hom(source: Int, target: SType): SType = Arrow(Parameter(source), target)

  def hom(source: SType, target: Int): SType = Arrow(source, Parameter(target))

  def hom(source: Int, target: Int): SType = Arrow(Parameter(source), Parameter(target))

  private def string(target: SType, sources: List[SType]): String = target match {
    case Arrow(s, t) => string(t, s :: sources)
    case Parameter(i) =>
      if (sources.isEmpty) i.toString else (i.toString :: sources.map(string(_, Nil))).reverse.mkString("(", " -> ", ")")
  }

  @tailrec def solve(in: List[(SType, SType)], out: Map[Int, SType]): Option[Map[Int, SType]] = in match {
    case Nil => Some(out)
    case h :: t => h match {
      case (Parameter(i), Parameter(j)) if i == j => solve(t, out)
      case (Parameter(i), x) if !x.parameters(i) => out get i match {
        case Some(y) => solve((y, x) :: t, out)
        case None =>
          val in2 = t.map { case (a, b) => (a.replace(i, x), b.replace(i, x)) }
          val out2 = out.mapValues( _.replace(i, x) ) + (i -> x)
          solve(in2, out2)
      }
      case (_, Parameter(_)) => solve(h.swap :: t, out)
      case (Arrow(a, b), Arrow(c, d)) => solve((a, c) :: (b, d) :: t, out)
      case _ => None
    }
  }

  case class Arrow(source: SType, target: SType) extends SType {
    override def replace(i: Int, x: SType): SType = Arrow(source.replace(i, x), target.replace(i, x))

    override def shift(m: Int): SType = Arrow(source.shift(m), target.shift(m))

    override def parameters: Set[Int] = source.parameters ++ target.parameters
  }

  case class Parameter(index: Int) extends SType {
    override def replace(i: Int, x: SType): SType = if (i == index) x else this

    override def shift(m: Int): SType = Parameter(index + m)

    override def parameters: Set[Int] = Set(index)
  }

}

class STerm private (val size: Int, free: Set[(Int, Int)], val types: Map[Int, SType]) {

  def getType: SType = typeOf(size - 1)

  private def typeOf(i: Int): SType = types.getOrElse(i, Parameter(i))

  def apply(other: STerm): Option[STerm] = {
    val f = free ++ other.free.map { case (i, p) => (i, p + size) }
    val opt = SType.solve(
      (getType, Arrow(other.getType.shift(size), Parameter(size + other.size))) :: Nil,
      types ++ other.types.map { case (i, x) => (i + size, x.shift(size)) })
    opt.map(t => new STerm(size + other.size + 1, f, t))
  }

  def lambda: Option[STerm] = {
    val f = free.collect { case (i, p) if i > 0 => (i - 1, p) }
    val p0 = free.collect { case (0, p) => p }
    if (p0.nonEmpty) {
      val opt = p0.foldLeft[Option[Map[Int, SType]]](Some(types)) {
        case (Some(r), p) => SType.solve((Parameter(size), Arrow(typeOf(p), getType)) :: Nil, r)
        case (None, _) => None
      }
      opt.map(new STerm(size + 1, f, _))
    } else {
      val opt = SType.solve((Parameter(size + 1), Arrow(Parameter(size), getType)) :: Nil, types)
      opt.map(new STerm(size + 2, f, _))
    }
  }

}

object STerm {
  def variable(index: Int): STerm = new STerm(1, Set(index -> 0), Map.empty)

  implicit val instance: UntypedLambdaTerm[Option[STerm]] = new UntypedLambdaTerm[Option[STerm]] {
    override def abstraction(term: Option[STerm]): Option[STerm] = term.flatMap(_.lambda)

    override def application(operator: Option[STerm], operand: Option[STerm]): Option[STerm] = for {
      x <- operator
      y <- operand
      z <- x(y)
    } yield z

    override def variable(index: Int): Option[STerm] = Some(STerm.variable(index))
  }

}

