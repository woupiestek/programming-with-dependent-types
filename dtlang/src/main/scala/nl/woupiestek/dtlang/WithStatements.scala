package nl.woupiestek.dtlang

object WithStatements {

  type Identifier = String

  sealed trait Statement

  case class SetType(identifier: Identifier, sort: Expression)
      extends Statement

  case class SetValue(identifier: Identifier, value: Expression)
      extends Statement

  sealed trait Expression {
    def meets(x: Expression): Effective[Unit]

    def getType: Effective[Expression]

    def getValue: Effective[Expression]

    def applyTo(x: Expression): Effective[Expression]
  }

  case class Variable(identifier: Identifier)
      extends Expression {
    override def getType: Effective[Expression] = Effective {
      Effective.getType(_, identifier)
    }

    override def getValue: Effective[Expression] = Effective {
      Effective.getValue(_, identifier)
    }

    override def meets(u: Expression): Effective[Unit] =
      u match {
        case Variable(i) if i == identifier =>
          Effective.point(())
        case _ =>
          for {
            v <- getValue
            w <- u.getValue
            _ <- w.meets(v)
          } yield ()
      }

    override def applyTo(x: Expression): Effective[Expression] =
      for {
        Product(a, b, c) <- getType
        d <- x.getType
        _ <- b meets d
      } yield Application(this, x)

  }

  case class Application(
      operator: Expression,
      operand: Expression
  ) extends Expression {
    override def getType: Effective[Expression] =
      for {
        (i, v) <- check
        _ <- Effective.set(SetValue(i, operand))
        w <- v.getValue
      } yield w

    override def getValue: Effective[Expression] =
      operator.applyTo(operand)

    private def check =
      for {
        Product(i, t, v) <- operator.getType
        u <- operand.getType
        _ <- t meets u
      } yield (i, v)

    override def meets(u: Expression): Effective[Unit] =
      for {
        a <- getValue
        b <- u.getValue
        _ <- (a, b) match {
          case (Application(c, d), Application(e, f)) =>
            for {
              _ <- c meets e
              _ <- d meets f
            } yield ()
          case (c, d) => c meets d
        }
      } yield ()

    override def applyTo(w: Expression): Effective[Expression] =
      for {
        x <- getValue
        y <- x.applyTo(w)
      } yield y
  }

  case class Abstraction(
      identifier: Identifier,
      dom: Expression,
      value: Expression
  ) extends Expression {
    override def getType: Effective[Expression] =
      for {
        _ <- Effective.set(SetType(identifier, dom))
        d <- dom.getValue
        x <- value.getType
      } yield Product(identifier, d, x)

    override def getValue: Effective[Expression] =
      Effective.point(this)

    override def meets(u: Expression): Effective[Unit] =
      for {
        Abstraction(j, e, w) <- u.getValue
        _ <- Effective.set(SetValue(j, Variable(identifier)))
        _ <- dom meets e
        _ <- value meets w
      } yield ()

    override def applyTo(w: Expression): Effective[Expression] =
      for {
        t <- w.getType
        _ <- dom meets t
        _ <- Effective.set(SetValue(identifier, w))
        u <- value.getValue
      } yield u
  }

  case class Assignment(
      identifier: Identifier,
      value: Expression,
      continuation: Expression
  ) extends Expression {
    override def getType: Effective[Expression] =
      for {
        _ <- Effective.set(SetValue(identifier, value))
        z <- continuation.getType
      } yield z

    override def getValue: Effective[Expression] =
      for {
        _ <- Effective.set(SetValue(identifier, value))
        z <- continuation.getValue
      } yield z

    override def meets(u: Expression): Effective[Unit] =
      for {
        x <- getValue
        _ <- x meets u
      } yield ()

    override def applyTo(w: Expression): Effective[Expression] =
      for {
        _ <- Effective.set(SetValue(identifier, value))
        x <- continuation.applyTo(w)
      } yield x
  }

  case class Product(
      identifier: Identifier,
      dom: Expression,
      value: Expression
  ) extends Expression {
    override def getType: Effective[Expression] =
      for {
        Universe(c) <- dom.getType
        Universe(d) <- value.getType
      } yield Universe(Math.max(c, d))

    override def getValue: Effective[Expression] =
      for {
        d <- dom.getValue
        v <- value.getValue
      } yield Product(identifier, d, v)

    override def meets(u: Expression): Effective[Unit] =
      for {
        Product(a, b, c) <- getValue
        Product(d, e, f) <- u.getValue
        _ <- Effective.set(SetValue(d, Variable(a)))
        _ <- b meets e
        _ <- c meets f
      } yield ()

    override def applyTo(w: Expression): Effective[Expression] =
      Effective(_ => None)
  }

  case class Universe(level: Int) extends Expression {
    override def getType: Effective[Expression] =
      Effective.point(Universe(level + 1))

    override def getValue: Effective[Expression] =
      Effective.point(this)

    override def meets(u: Expression): Effective[Boolean] =
      Effective.point(this == u)

    override def applyTo(w: Expression): Effective[Expression] =
      Effective(_ => None)

  }

  class Effective[X](
      val run: List[Statement] => Option[(List[Statement], X)]
  ) {
    def map[Y](f: X => Y) = Effective { context =>
      for ((c, x) <- run(context)) yield (c, f(x))
    }

    def flatMap[Y](f: X => Effective[Y]) = Effective {
      context =>
        for {
          (a, b) <- run(context)
          c <- f(b).run(a)
        } yield c
    }

    def withFilter(p: X => Boolean): Effective[X] = Effective {
      context =>
        for {
          (l, x) <- run(context) if p(x)
        } yield (l, x)
    }
  }

  object Effective {
    def apply[X](
        effect: List[Statement] => Option[(List[Statement], X)]
    ): Effective[X] = new Effective[X](effect)

    def point[X](x: X): Effective[X] = apply { context =>
      Some((context, x))
    }

    def set(statement: Statement): Effective[Unit] = apply {
      context =>
        Some((statement :: context, ()))
    }

    def getType(
        context: List[Statement],
        identifier: Identifier
    ): Option[(List[Statement], Expression)] = context match {
      case Nil => None
      case SetType(j, e) :: tail if j == identifier =>
        e.getValue.run(tail)
      case SetValue(j, e) :: tail if j == identifier =>
        e.getType.run(tail)
      case _ :: tail => getType(tail, identifier)
    }

    def getValue(
        context: List[Statement],
        identifier: Identifier
    ): Option[(List[Statement], Expression)] = context match {
      case Nil => Some((Nil, Variable(identifier)))
      case SetValue(j, e) :: tail if j == identifier =>
        e.getValue.run(tail)
      case _ :: tail => getValue(tail, identifier)
    }
  }

}
