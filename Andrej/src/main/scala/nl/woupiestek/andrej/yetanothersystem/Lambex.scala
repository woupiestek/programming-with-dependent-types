package nl.woupiestek.andrej.yetanothersystem


trait Lambex[E] {
  def variable(identifier: String): E

  def cut(identifier: String, value: E, body: E): E

  def lambda(identifier: String, body: E): E

  def application(operator: E, operand: E): E
}

object Lambex {

  sealed trait LNode

  case class AppL(stack: List[LNode], variable: String) extends LNode

  case class AbsL(heap: Map[String, LNode], identifier: String, body: LNode) extends LNode

  object Instance extends Lambex[LNode] {
    override def variable(identifier: String): LNode = AppL(Nil, identifier)

    def cut(heap: Map[String, LNode], body: LNode): LNode = body match {
      case AbsL(h, i, b) => AbsL(heap ++ h.mapValues(cut(heap, _)), i, b)
      case AppL(s, v) => s.foldRight(heap.getOrElse(v, variable(v))) { case (x, y) => application(y, cut(heap, x)) }
    }

    override def cut(identifier: String, value: LNode, body: LNode): LNode = cut(Map(identifier -> value), body)

    override def lambda(identifier: String, body: LNode): LNode = AbsL(Map.empty, identifier, body)

    override def application(operator: LNode, operand: LNode): LNode = operator match {
      case AppL(s, v) => AppL(operand :: s, v)
      case AbsL(h, i, b) => cut(h + (i -> operand), b)
    }
  }

  case class L(f: L => Option[L])

  object Instance2 extends Lambex[Map[String, L] => Option[L]] {
    override def variable(identifier: String): (Map[String, L]) => Option[L] = _.get(identifier)

    override def cut(identifier: String, value: (Map[String, L]) => Option[L], body: (Map[String, L]) => Option[L]): (Map[String, L]) => Option[L] =
      context => body(context + (identifier -> value(context)))

    override def lambda(identifier: String, body: (Map[String, L]) => Option[L]): (Map[String, L]) => Option[L] =
      context => Some(L(x => body(context + (identifier -> x))))

    override def application(operator: (Map[String, L]) => Option[L], operand: (Map[String, L]) => Option[L]): (Map[String, L]) => Option[L] =
      context => for {
        L(f) <- operator(context)
        x <- operand(context)
        y <- f(x)
      } yield y
  }

}

trait DeBruijnex[E] {
  def variable(index: Int): E

  def cut(value: E, body: E): E

  def lambda(body: E): E

  def application(operator: E, operand: E): E
}

class DeBruijnizer[E](db: DeBruijnex[E]) extends Lambex[List[String] => Option[E]] {
  override def variable(identifier: String): (List[String]) => Option[E] =
    list => Some(list.indexOf(identifier)).collect {
      case i if i >= 0 => db.variable(i)
    }

  override def cut(identifier: String, value: (List[String]) => Option[E], body: (List[String]) => Option[E]): (List[String]) => Option[E] =
    list => for {
      v <- value(list)
      b <- body(identifier :: list)
    } yield db.cut(v, b)

  override def lambda(identifier: String, body: (List[String]) => Option[E]): (List[String]) => Option[E] =
    list => body(identifier :: list).map(db.lambda)

  override def application(operator: (List[String]) => Option[E], operand: (List[String]) => Option[E]): (List[String]) => Option[E] =
    list => for {
      x <- operator(list)
      y <- operand(list)
    } yield db.application(x, y)
}

object DeBruijnex {

  sealed trait Node

  case class AppN(arguments: List[Node], variable: Int) extends Node

  case class AbsN(context: List[Node], body: Node) extends Node

  object Instance extends DeBruijnex[Node] {

    override def variable(index: Int): Node = AppN(Nil, index)

    override def cut(value: Node, body: Node): Node = body match {
      case AppN(args, 0) => args.foldRight(value) { case (x, y) => application(y, cut(value, x)) }
      case AppN(args, n) if n > 0 => AppN(args.map(cut(value, _)), n - 1)
      case AbsN(bs, c) => AbsN(value :: bs, c)
    }

    override def lambda(body: Node): Node = AbsN(Nil, body)

    override def application(operator: Node, operand: Node): Node = operator match {
      case AppN(args, i) => AppN(operand :: args, i)
      case AbsN(bs, c) => bs.foldRight(cut(operand, c)) { case (x, y) => cut(x, y) }
    }
  }

  case class L(f: L => Option[L])

  object Instance2 extends DeBruijnex[List[L] => Option[L]] {
    override def variable(index: Int): (List[L]) => Option[L] = _ lift index

    override def cut(value: (List[L]) => Option[L], body: (List[L]) => Option[L]): (List[L]) => Option[L] =
      list => body(value(list) :: list)

    override def lambda(body: (List[L]) => Option[L]): (List[L]) => Option[L] = tail => Some(L(head => body(head :: tail)))

    override def application(operator: (List[L]) => Option[L], operand: (List[L]) => Option[L]): (List[L]) => Option[L] =
      list => for {
        L(f) <- operator(list)
        x <- operand(list)
        y <- f(x)
      } yield y
  }

  case class La(f: La => La)

  object Instance3 extends DeBruijnex[(Int => La) => La] {
    override def variable(index: Int): ((Int) => La) => La = _.apply(index)

    override def cut(value: ((Int) => La) => La, body: ((Int) => La) => La): ((Int) => La) => La =
      context => body(i => if (i > 0) context(i - 1) else value(context))

    override def lambda(body: ((Int) => La) => La): ((Int) => La) => La =
      context => La(x => body(i => if (i > 0) context(i - 1) else x))

    override def application(operator: ((Int) => La) => La, operand: ((Int) => La) => La): ((Int) => La) => La =
      context => operator(context).f(operand(context))
  }

}



