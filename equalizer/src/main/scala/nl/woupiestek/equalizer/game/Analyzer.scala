package nl.woupiestek.equalizer.game

object Analyzer {

  case class Model(
      private val model0: Map[Int, Int],
      private val model1: Map[Int, (Int, Int)]
  ) {
    def add(i: Int, j: Int, k: Int): Model = model1.get(i) match {
      case None           => copy(model1 = model1 + (i -> (j -> k)))
      case Some((j0, k0)) => equate(j0, j).equate(k0, k)
    }

    def equate(i: Int, j: Int): Model = {
      (model0.get(i), model0.get(j)) match {
        case (None, None) =>
          if (i > j) copy(model0 = model0 + (i -> j))
          else if (j > i) copy(model0 = model0 + (j -> i))
          else this
        case (None, Some(k)) => copy(model0 = model0 + (i -> k) + (j -> k))
        case (Some(k), None) => copy(model0 = model0 + (i -> k) + (j -> k))
        case (Some(k), Some(l)) =>
          if (k > l) copy(model0 = model0 + (i -> l) + (k -> l))
          else if (l > k) copy(model0 = model0 + (j -> k) + (l -> k))
          else this
      }
    }

    def merge(m: Model): Model = {
      val with0 = m.model0.view.foldLeft(this)((n, q) => n.equate(q._1, q._2))
      val with1 =
        m.model1.foldLeft(with0)((n, q) => n.add(q._1, q._2._1, q._2._2))
      with1
    }
  }

  object Model {
    val empty: Model = Model(Map.empty, Map.empty)
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
              .fold(Model.empty)(Model.empty.equate(e.index, _))
          )
      )
  }

}
