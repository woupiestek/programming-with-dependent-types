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
      equations: List[(Type, Type)],
      context: Map[String, Type],
      value: Type
  )

  def types: Result[(Int => Char), Nothing, Sequent] = {
    lazy val cut: Result[(Int => Char), Nothing, Sequent] =
      (for {
        _ <- token("[")
        a <- identifier
        _ <- token("=")
        b <- cut
        _ <- token("]")
        c <- cut
      } yield
        Sequent(
          b.equations ++ c.equations ++ c.context
            .get(a)
            .map((_, b.value)),
          b.context ++ (c.context - a),
          c.value
        )) ++ intro

    lazy val intro: Result[(Int => Char), Nothing, Sequent] =
      (for {
        a <- Result.position[(Int => Char), Nothing]
        b <- identifier
        _ <- token("->")
        d <- cut
      } yield {
        val area = TypeOf(a)
        Sequent(
          d.equations ++ d.context.get(b).map((area, _)),
          d.context - b,
          Arrow(area, d.value)
        )
      }) ++ elim.map {
        case (c, d) =>
          val equations = c.equations ++ d.flatMap(_.equations)
          val context = c.context ++ d.flatMap(_.context)
          val value = d.foldLeft(c.value)(
            (typ, seq) => Arrow(seq.value, typ)
          )
          Sequent(equations, context, value)
      }

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
        } yield {
          val area = TypeOf(a)
          Sequent(Nil, Map(b -> area), area)
        })

    cut
  }

}
