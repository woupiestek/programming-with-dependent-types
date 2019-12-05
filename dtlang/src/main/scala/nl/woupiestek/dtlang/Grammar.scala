package nl.woupiestek.dtlang

object Grammar {

  def whitespace: Result[(Int => Char), Nothing, Unit] =
    Result((r, i) => {
      var j = i
      while (Character.isWhitespace(r(j))) j += 1
      Some((Right(()), j))
    })

  def token(
      string: String
  ): Result[(Int => Char), Nothing, Unit] =
    Result(
      (r, i) =>
        if (string.zipWithIndex.forall {
              case (c, j) => c == r(i + j)
            }) {
          var k = i + string.length()
          while (Character.isWhitespace(r(k))) k += 1
          Some((Right(()), k))
        } else {
          None
        }
    )

  def identifier: Result[(Int => Char), Nothing, String] =
    Result((r, i) => {
      var j = i
      while (('a' <= r(i) && r(i) <= 'z')
             || ('A' <= r(i) && r(i) <= 'Z')) j += 1
      var k = j
      while (Character.isWhitespace(r(k))) k += 1
      Some((Right((i to j).map(r).mkString), k))
    })

  trait Expression[E] {
    def let(x: String, y: E, z: E): E
    def abst(x: String, y: E): E
    def appl(x: E, y: List[E]): E
    def vari(x: String): E
  }

  def expression[E](
      E: Expression[E]
  ): Result[(Int => Char), Nothing, E] = {
    lazy val cut: Result[(Int => Char), Nothing, E] =
      (for {
        _ <- token("[")
        a <- identifier
        _ <- token("=")
        b <- cut
        _ <- token("]")
        c <- cut
      } yield E.let(a, b, c)) ++ intro

    lazy val intro: Result[(Int => Char), Nothing, E] =
      (for {
        a <- identifier
        _ <- token("->")
        b <- cut
      } yield E.abst(a, b)) ++ elim.map {
        case (c, d) => E.appl(c, d)
      }

    lazy val elim
        : Result[(Int => Char), Nothing, (E, List[E])] =
      for {
        a <- unit
        b <- elim.map { case (c, d) => c :: d } ++
          Result.point(Nil)
      } yield (a, b)

    lazy val unit: Result[(Int => Char), Nothing, E] =
      (for {
        _ <- token("(")
        a <- cut
        _ <- token(")")
      } yield a) ++ identifier.map(E.vari)

    cut
  }

  sealed abstract class Type
  case class TypeOf(position: Int) extends Type
  case class Arrow(tail: Type, head: Type) extends Type

  case class Sequent(
      model: Model,
      context: Map[String, Int],
      value: Type
  ) {
    def weaken(a: String, b: Int) =
      context.get(a) match {
        case None    => copy(context = context + (a -> b))
        case Some(c) => copy(model = model.add(c, TypeOf(b)))
      }
  }

  def let(a: String, b: Sequent, c: Sequent): Sequent = {
    val model1 = b.model.addAll(c.model)
    (c.context - a).foldLeft(
      Sequent(
        c.context
          .get(a)
          .map(model1.add(_, b.value))
          .getOrElse(model1),
        b.context,
        c.value
      )
    ) {
      case (d, (e, f)) => d.weaken(e, f)
    }
  }

  def appl(c: Sequent, d: Sequent): Sequent =
    c.context.foldLeft(
      Sequent(
        c.model.addAll(d.model),
        c.context,
        Arrow(d.value, c.value)
      )
    ) {
      case (seq, (e, f)) => seq.weaken(e, f)
    }

  case class Model(types: Map[Int, Type] = Map.empty)
      extends AnyVal {
    def add(i: Int, t: Type): Model = {
      types.get(i) match {
        case None    => Model(types + (i -> t))
        case Some(u) => compareAndAddResults(u, t)
      }
    }

    def compareAndAddResults(t: Type, u: Type): Model = {
      t match {
        case Arrow(tail, head) =>
          u match {
            case Arrow(tail2, head2) =>
              compareAndAddResults(tail, tail2)
                .compareAndAddResults(head, head2)
            case TypeOf(position) => add(position, t)
          }
        case TypeOf(position) => add(position, u)
      }
    }

    def addAll(other: Model) = other.types.foldLeft(this) {
      case (m, (i, t)) => m.add(i, t)
    }
  }

  def types: Result[(Int => Char), Nothing, Sequent] = {
    lazy val cut: Result[(Int => Char), Nothing, Sequent] =
      (for {
        _ <- token("[")
        a <- identifier
        _ <- token("=")
        b <- cut
        _ <- token("]")
        c <- cut
      } yield let(a, b, c)) ++ intro

    lazy val intro: Result[(Int => Char), Nothing, Sequent] =
      (for {
        a <- Result.position[(Int => Char), Nothing]
        b <- identifier
        _ <- token("->")
        d <- cut
      } yield {
        val area = TypeOf(a)
        Sequent(
          d.context
            .get(b)
            .map(d.model.add(_, area))
            .getOrElse(d.model),
          d.context - b,
          Arrow(area, d.value)
        )
      }) ++ elim.map { case (c, d) => d.foldLeft(c)(appl) }

    lazy val elim: Result[
      (Int => Char),
      Nothing,
      (Sequent, List[Sequent])
    ] =
      for {
        a <- unit
        c <- elim.map { case (c, d) => c :: d } ++
          Result.point(Nil)
      } yield (a, c)

    lazy val unit: Result[(Int => Char), Nothing, Sequent] =
      (for {
        _ <- token("(")
        a <- cut
        _ <- token(")")
      } yield a) ++
        (for {
          a <- Result.position[(Int => Char), Nothing]
          b <- identifier
        } yield Sequent(Model(), Map(b -> a), TypeOf(a)))

    cut
  }

}
