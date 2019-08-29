package nl.woupiestek.andrej.equalambda

trait WeakHeadNormalForm[W, T] {
  def lambda(
      name: String,
      term: T,
      substitution: Map[String, T]
  ): W

  def alpha(name: String, operands: List[T]): W

  case class Builder(
      value: T,
      buildWith: (Map[String, Builder], List[Builder]) => W
  ) {
    def build: W = buildWith(Map.empty, Nil)
  }

  implicit def reduce(implicit T: Term[T]): Term[Builder] =
    new Term[Builder] {
      override def identifier(name: String): Builder =
        Builder(
          T.identifier(name),
          (context, args) =>
            if (context.contains(name))
              context(name).buildWith(Map.empty, args)
            else alpha(name, args.map(_.value))
        )

      override def where(
          term: Builder,
          substitution: Map[String, Builder]
      ): Builder =
        Builder(
          T.where(term.value, substitution.mapValues(_.value)),
          (context, args) =>
            term.buildWith(context ++ substitution, args)
        )

      override def application(
          operator: Builder,
          operands: List[Builder]
      ): Builder =
        Builder(
          T.application(operator.value, operands.map(_.value)),
          (context, args) => {
            operator.buildWith(
              context,
              operands.map(where(_, context)) ++ args
            )
          }
        )

      override def abstraction(
          arg: String,
          body: Builder
      ): Builder =
        Builder(
          T.abstraction(arg, body.value),
          (context, args) =>
            args match {
              case Nil =>
                lambda(
                  arg,
                  body.value,
                  context.mapValues(_.value)
                )
              case h :: t =>
                body.buildWith(context + (arg -> h), t)
            }
        )
    }

}
