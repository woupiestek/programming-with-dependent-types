package nl.woupiestek.dtlang

object Grammar {

  def advance(
      input: Int => Char,
      condition: Char => Boolean,
      start: Int
  ): Int = {
    var j = start
    while (condition(input(j))) j += 1
    j
  }

  def whitespace: Result[(Int => Char), Unit] =
    Result[(Int => Char), Unit]((r, i) => {
      val j = advance(r, Character.isWhitespace, i)
      Some(((), j))
    }).memoized

  def token(
      string: String
  ): Result[(Int => Char), Unit] =
    Result(
      (r, i) =>
        if ((0 until string.length()).forall(
              j => r(i + j) == string.charAt(j)
            )) {
          val k = advance(
            r,
            Character.isWhitespace,
            i + string.length()
          )
          Some(((), k))
        } else {
          None
        }
    )

  def identifier: Result[(Int => Char), String] =
    Result[(Int => Char), String]((r, i) => {
      val j = advance(
        r,
        c =>
          ('a' <= c && c <= 'z')
            || ('A' <= c && c <= 'Z'),
        i
      )
      if(j == i) {None} else {
      val k = advance(r, Character.isWhitespace, j)
      Some(((i until j).map(r).mkString, k))
      }
    }).memoized

  type R[A] = Result[(Int => Char), A]

  trait Expression[E] {
    def let(x: String, y: E, z: E): E
    def abst(y: String, z: E): E
    def appl(y: E, z: E): E
    def vari(x: String): E
  }

  def expression[E](
      E: Expression[E]
  ): R[E] = {
    lazy val cut: R[E] =
      (for {
        _ <- token("[")
        a <- identifier
        _ <- token("=")
        b <- cut
        _ <- token("]")
        c <- cut
      } yield E.let(a, b, c)) ++ intro

    lazy val intro: R[E] =
      (for {
        b <- identifier
        _ <- token("->")
        c <- intro
      } yield E.abst(b, c)) ++ elim.map {
        case (c, d) => d.foldLeft(c)(E.appl)
      }

    lazy val elim: R[(E, List[E])] =
      for {
        b <- unit
        c <- elim.map { case (d, e) => d :: e } ++
          Result.point(Nil)
      } yield (b, c)

    lazy val unit: R[E] =
      (for {
        _ <- token("(")
        a <- cut
        _ <- token(")")
      } yield a) ++
        identifier.map(E.vari)

    cut.memoized
  }

  sealed abstract class Type
  case class TypeOf(position: Int) extends Type
  case class Arrow(tail: Type, head: Type) extends Type

  case class Model(
      types: Map[Int, Type],
      context: Map[String, Int]
  )

  val asExpression: Expression[Int => (Int, Model)] =
    new Expression[Int => (Int, Model)] {
      def abst(
          y: String,
          z: Int => (Int, Model)
      ): Int => (Int, Model) = {
        (i: Int) => {
          val (j, m) = z(i)
          val types = solve(
            (TypeOf(j + 1), Arrow(TypeOf(j), TypeOf(j - 1))) ::
              m.context
                .get(y)
                .map(k => ((TypeOf(j), TypeOf(k))))
                .toList,
            m.types
          )

          (j + 2, Model(types, m.context - y))
        }
      }

      def appl(
          y: Int => (Int, Model),
          z: Int => (Int, Model)
      ): Int => (Int, Model) = {
        (i: Int) => {
          val (j, m) = y(i)
          val (k, n) = z(j)
          val types = solve(
            n.types.toList.map {
              case (l, t) => (TypeOf(l), t)
            } ++ n.context.toList.flatMap {
              case (l, t) =>
                m.context
                  .get(l)
                  .map(u => (TypeOf(u), TypeOf(t)))
            },
            m.types
          )
          (k + 1, Model(types, m.context ++ n.context))
        }
      }

      def let(
          x: String,
          y: Int => (Int, Model),
          z: Int => (Int, Model)
      ): Int => (Int, Model) = {
        (i: Int) => {
          val (j, m) = y(i)
          val (k, n) = z(j)
          val equations: List[(Type, Type)] = m.types.toList
            .map { case (l, t) => (TypeOf(l), t) } ++ n.context.toList
            .flatMap {
              case (l, t) if l != x =>
                m.context
                  .get(l)
                  .map(u => (TypeOf(u), TypeOf(t)))
            }
          val context = m.context ++ (n.context - x)
          val types = solve(equations, m.types ++ n.types)
          (k, Model(types, context))
        }
      }

      def vari(x: String): Int => (Int, Model) = {
        i => (i + 1, Model(Map.empty, Map(x -> i)))
      }
    }

  private def solve(
      equations: List[(Type, Type)],
      types: Map[Int, Type]
  ): Map[Int, Type] = equations match {
    case Nil => types
    case (a, b) :: c =>
      a match {
        case Arrow(tail, head) =>
          b match {
            case Arrow(tail2, head2) =>
              solve(
                (tail, tail2) :: (head, head2) :: c,
                types
              )
            case TypeOf(position) =>
              types.get(position) match {
                case None => solve(c, types + (position -> a))
                case Some(value) =>
                  solve((a, value) :: c, types)
              }
          }
        case TypeOf(position) =>
          types.get(position) match {
            case None        => solve(c, types + (position -> b))
            case Some(value) => solve((value, b) :: c, types)
          }
      }
  }

  //add parenteses based on context precedence...
  val asString: Expression[Int => String] = new Expression[Int => String] {
    def abst(y: String,z: Int => String): Int => String = {
      val w = s"$y -> ${z(2)}"
      i => if(i >= 2) w else s"($w)"
    }
    def appl(y: Int => String, z: Int => String): Int => String = {      
      val w = s"${y(1)}${z(0)}"
      i => if(i >= 1) w else s"($w)"
    }
  
    def let(x: String,y: Int => String,z: Int => String): Int => String = 
      {
        val w = s"[$x = ${y(3)}]${z(3)}"
        i => if(i == 3) w else s"($w)"
      }
    def vari(x: String): Int => String = i => if(i > 0) x else " "+x
  }



}
