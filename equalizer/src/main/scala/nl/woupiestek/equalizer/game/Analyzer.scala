package nl.woupiestek.equalizer.game

object Analyzer {

  case class Model(
      model0: List[(Int, Int)],
      model1: List[(Int, Int, Int)]
  ) {
    def add(i: Int, j: Int, k: Int): Model = copy(model1 = (i, j, k) :: model1)
    def equate(i: Int, j: Int): Model = copy(model0 = (i, j) :: model0)
    def merge(m: Model): Model = Model(model0 ++ m.model0, model1 ++ m.model1)
  }

  object Model {
    val empty: Model = Model(Nil, Nil)
  }

  final case class E(context: Map[String, Int], index: Int) {
    def mark(name: String) = E(context + (name -> index), index + 1)
  }

  type T = Reader[E, (Int, Model)]
  object TypeOf {

    def abstraction(varName: String, body: T): T =
      Reader((_: E).index).flatMap(
        (x: Int) =>
          body
            .local((e: E) => E(e.context + (varName -> x), x + 1))
            .map((y: (Int, Model)) => (y._1 + 1, y._2.add(y._1 + 1, x, y._1)))
      )

    def application(operator: T, operand: T): T =
      operator.flatMap(
        (x: (Int, Model)) =>
          operand
            .local((_: E).copy(index = x._1 + 1))
            .map(
              (y: (Int, Model)) =>
                (y._1 + 1, x._2.merge(y._2).add(x._1, y._1, y._1 + 1))
            )
      )

    def let(x: String, y: T, z: T): T =
      Reader((_: E).index).flatMap(
        (u: Int) =>
          y.local((e: E) => e.copy(index = u + 1))
            .flatMap(
              (v: (Int, Model)) =>
                z.local(
                    (e: E) => E(e.context + (x -> u), v._1 + 1)
                  )
                  .map(
                    (w: (Int, Model)) =>
                      (
                        w._1 + 1,
                        v._2
                          .merge(w._2)
                          .equate(u, v._1)
                          .equate(w._1, w._1 + 1)
                      )
                  )
            )
      )

    def variable(name: String): T =
      Reader(
        (e: E) =>
          (
            e.index,
            e.context
              .get(name)
              .fold(Model(Nil, Nil))(j => Model(List((e.index, j)), Nil))
          )
      )
  }

}
