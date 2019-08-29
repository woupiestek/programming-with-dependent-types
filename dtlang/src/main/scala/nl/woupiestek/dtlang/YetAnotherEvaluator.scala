package nl.woupiestek.dtlang

object YetAnotherEvaluator {

  type Identifier = String

  trait Interpreter[T] {
    def variable(name: Identifier): T

    def alpha(operator: T, operand: T): T

    def lambda(variable: Identifier, domain: T, body: T): T

    def product(variable: Identifier, domain: T, body: T): T

    //avoid capture of variable in body
    def release(variable: Identifier, body: T): T

    def universe(level: Int): T
  }

  sealed trait Expression {
    def evaluate[T](e: Interpreter[T]): T
  }

  case class Variable(name: Identifier) extends Expression {
    override def evaluate[T](e: Interpreter[T]): T =
      e.variable(name)
  }

  case class Application(
      operator: Expression,
      operand: Expression
  ) extends Expression {
    override def evaluate[T](e: Interpreter[T]): T =
      e.alpha(operator.evaluate(e), operand.evaluate(e))
  }

  case class Abstraction(
      variable: Identifier,
      domain: Expression,
      body: Expression
  ) extends Expression {
    override def evaluate[T](e: Interpreter[T]): T =
      e.lambda(variable, domain.evaluate(e), body.evaluate(e))
  }

  case class Product(
      variable: Identifier,
      domain: Expression,
      body: Expression
  ) extends Expression {
    override def evaluate[T](e: Interpreter[T]): T =
      e.product(variable, domain.evaluate(e), body.evaluate(e))
  }

  case class Universe(level: Int) extends Expression {
    override def evaluate[T](e: Interpreter[T]): T =
      e.universe(level)
  }

  case class Release(variable: Identifier, body: Expression)
      extends Expression {
    override def evaluate[T](e: Interpreter[T]): T =
      e.release(variable, body.evaluate(e))
  }

  class Reduce(key: Identifier, value: Expression)
      extends Interpreter[Option[Expression]] {
    override def variable(
        name: Identifier
    ): Option[Expression] =
      Some(if (key == name) value else Variable(name))

    override def universe(level: Int): Option[Expression] =
      Some(Universe(level))

    override def alpha(
        operator: Option[Expression],
        operand: Option[Expression]
    ): Option[Expression] =
      for {
        x <- operator
        Abstraction(a, _, b) <- x.evaluate(this)
        y <- operand
        c <- y.evaluate(this)
        z <- b.evaluate(new Reduce(a, c))
      } yield z

    override def product(
        variable: Identifier,
        domain: Option[Expression],
        body: Option[Expression]
    ): Option[Expression] =
      for {
        d <- domain
        e <- d.evaluate(this)
        b <- body
        c <- b.evaluate(
          new Reduce(key, Release(variable, value))
        )
      } yield Product(variable, e, c)

    override def lambda(
        variable: Identifier,
        domain: Option[Expression],
        body: Option[Expression]
    ): Option[Expression] =
      for {
        d <- domain
        e <- d.evaluate(this)
        b <- body
        c <- b.evaluate(
          new Reduce(key, Release(variable, value))
        )
      } yield Abstraction(variable, e, c)

    //avoid capture of variable in body
    override def release(
        variable: Identifier,
        body: Option[Expression]
    ): Option[Expression] =
      if (key == variable) body
      else
        for {
          b <- body
          c <- b.evaluate(this)
        } yield Release(variable, c)
  }

  case class Context(heap: Map[Identifier, List[Expression]])
      extends Interpreter[Option[Expression]] {
    //    private def push(key: Identifier, value: Expression): Context = heap lift key match {
    //      case None => Context(heap + (key -> List(value)))
    //      case Some(list) => Context(heap + (key -> (value :: list)))
    //    }

    private def delete(key: Identifier): Context =
      heap lift key match {
        case Some(_ :: Nil)  => Context(heap - key)
        case Some(_ :: tail) => Context(heap + (key -> tail))
        case _               => this
      }

    private def peek(key: Identifier): Option[Expression] =
      heap lift key collect { case head :: _ => head }

    override def variable(
        name: Identifier
    ): Option[Expression] = peek(name)

    override def universe(level: Int): Option[Expression] =
      Some(Universe(level))

    override def alpha(
        operator: Option[Expression],
        operand: Option[Expression]
    ): Option[Expression] =
      for {
        a <- operator
        Abstraction(b, _, c) <- a.evaluate(this)
        d <- operand
        e <- d.evaluate(this)
        f <- c.evaluate(Context(Map(b -> List(e)))) //too bad
      } yield f

    override def product(
        variable: Identifier,
        domain: Option[Expression],
        body: Option[Expression]
    ): Option[Expression] =
      for {
        a <- domain
        b <- a.evaluate(this)
        c <- body
        d <- c.evaluate(delete(variable))
      } yield Product(variable, b, d)

    override def lambda(
        variable: Identifier,
        domain: Option[Expression],
        body: Option[Expression]
    ): Option[Expression] =
      for {
        a <- domain
        b <- a.evaluate(this)
        c <- body
        d <- c.evaluate(delete(variable))
      } yield Abstraction(variable, b, d)

    override def release(
        variable: Identifier,
        body: Option[Expression]
    ): Option[Expression] =
      for {
        a <- body
        b <- a.evaluate(delete(variable))
      } yield b
  }

}
