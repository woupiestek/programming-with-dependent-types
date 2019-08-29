package nl.woupiestek.equalizer.game

object LambdaMatcher {

  final case class E(
      heap: Map[String, Reader[E, Q]],
      stack: List[Reader[E, Q]],
      depth: Int
  ) {

    def push(r: Reader[E, Q]) = copy(stack = r :: stack)
    def lambda(key: String) = stack match {
      case h :: t => copy(heap = heap + (key -> h), stack = t)
      case Nil =>
        copy(
          heap = heap + (key -> fresh(depth)),
          depth = depth + 1
        )
    }
    def let(key: String, value: Reader[E, Q]) =
      copy(heap = heap + (key -> value))
    def get(key: String) =
      heap
        .get(key)
        .fold(Reader((e: E) => Q(Right(key), e.stack)))(
          _.local(_.copy(heap = Map.empty))
        )
  }

  final case class Q(
      varName: Either[Int, String],
      stack: List[Reader[E, Q]]
  )

  def fresh(depth: Int): Reader[E, Q] =
    Reader((_: E) => Q(Left(depth), Nil))

  implicit class Ops(val reader: Reader[E, Q]) {
    def closure: Reader[E, Reader[E, Q]] =
      Reader(
        e => reader.local(_.copy(heap = e.heap, stack = Nil))
      )
  }

  // probably not the correct way to get fresh variables
  object PInstance {
    def application(
        operator: Reader[E, Q],
        operand: Reader[E, Q]
    ): Reader[E, Q] =
      operand.closure.flatMap(
        (c: Reader[E, Q]) => operator.local(_.push(c))
      )
    def abstraction(
        varName: String,
        body: Reader[E, Q]
    ): Reader[E, Q] =
      body.local(_.lambda(varName))
    def let(
        varName: String,
        value: Reader[E, Q],
        body: Reader[E, Q]
    ) =
      value.closure.flatMap(
        (c: Reader[E, Q]) => body.local(_.let(varName, c))
      )
    def variable(name: String): Reader[E, Q] =
      Reader.get.flatMap((_: E).get(name))
  }

  //need a result type
  def equate(
      left: Reader[E, Q],
      right: Reader[E, Q]
  ): Reader[E, List[(Either[Int, String], Q)]] = {

    def traverse[X, Y](
        list: List[X]
    )(f: X => Reader[E, List[Y]]): Reader[E, List[Y]] =
      list match {
        case Nil => Reader.point[E, List[Y]](Nil)
        case h :: t =>
          for {
            h0: List[Y] <- f(h)
            t0: List[Y] <- traverse(t)(f)
          } yield h0 ++ t0
      }

    def next(
        ql: Q,
        e1: E,
        qr: Q,
        e2: E
    ): Reader[E, List[(Either[Int, String], Q)]] = {
      val i = ql.stack.length + e2.depth - e1.depth - qr.stack.length
      val fs = ql.stack ++ (e1.depth until e2.depth)
        .map(fresh)
        .toList
      if (i < 0) {
        traverse(
          fs.zip(qr.stack.drop(-i))
        ) {
          case (x: Reader[E, Q], y: Reader[E, Q]) =>
            equate(x, y)
        }.map(
          (t: List[(Either[Int, String], Q)]) =>
            ((ql.varName, qr.copy(stack = qr.stack.take(-i)))) :: t
        )
      } else {
        traverse(
          fs.drop(i)
            .zip(qr.stack)
        ) {
          case (x: Reader[E, Q], y: Reader[E, Q]) =>
            equate(x, y)
        }.map(
          (t: List[(Either[Int, String], Q)]) =>
            ((qr.varName, ql.copy(stack = fs.take(i)))) :: t
        )
      }
    }

    for {
      e0: E <- Reader.get[E]
      ql: Q <- left.local(_.copy(heap = Map.empty, stack = Nil))
      e1: E <- Reader.get[E]
      qr: Q <- right.local(
        _.copy(
          heap = Map.empty,
          stack = (e0.depth until e1.depth).map(fresh).toList
        )
      )
      e2: E <- Reader.get[E]
      qs: List[(Either[Int, String], Q)] <- next(ql, e1, qr, e2)
    } yield qs

  }

}
