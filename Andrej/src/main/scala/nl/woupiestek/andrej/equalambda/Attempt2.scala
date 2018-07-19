package nl.woupiestek.andrej.equalambda

object Attempt2 {

  trait Term[T] {
    def identifier(name: String): T

    def where(term: T, substitution: Map[String, T]): T

    def application(operator: T, operands: List[T]): T

    def abstraction(arg: String, body: T): T
  }

  trait TermWithQ[T] extends Term[T] {
    /* |-M=N
     * |-P:M=N->p
     * ----------
     * |-M=N?P:p
     */
    def equalizer(left: T, right: T, term: T): T

    /* |-M=N,
     * M=N|-P:p
     * --------------
     * |-M=N!P:M=N->p
     */
    def factor(left: T, right: T, term: T): T
  }

  abstract class Type[Ty, Te: Term] {

    def eq(left: Te, right: Te): Ty

    def product(name: String, dom: Ty, cod: Ty)

    def variable(index: Int): Ty

  }

  abstract class WeakHeadNormalForm[W, T](implicit T: Term[T]) {
    def lambda[Term](name: String, term: T, substitution: Map[String, T]): W

    def alpha[Term](name: String, operands: List[T]): W

    case class Builder(value: T, buildWith: (Map[String, Builder], List[T]) => W) {
      def build: W = buildWith(Map.empty, Nil)
    }

    object Reduce extends Term[Builder] {
      override def identifier(name: String): Builder = Builder(T.identifier(name), (context, args) =>
        if (context.contains(name)) context(name).buildWith(Map.empty, args)
        else alpha(name, args))

      override def where(term: Builder, substitution: Map[String, Builder]): Builder =
        Builder(T.where(term.value, substitution.mapValues(_.value)),
          (context, args) => term.buildWith(context ++ substitution, args))

      override def application(operator: Builder, operands: List[Builder]): Builder =
        Builder(T.application(operator.value, operands.map(_.value)), (context, args) => {
          val contextValues = context.mapValues(_.value)
          operator.buildWith(context, operands.map(x => T.where(x.value, contextValues)) ++ args)
        })

      override def abstraction(arg: String, body: Builder): Builder =
        Builder(T.abstraction(arg, body.value), (context, args) => args match {
          case Nil => lambda(arg, body.value, context.mapValues(_.value))
          case h :: t => body.buildWith(context + (arg -> h), t)
        })
    }

  }


  abstract class HeadNormalForm[H, T](implicit T: Term[T], mask: (String, T) => T) {
    def result(vars: List[String], arg: String, mask: Int, args: List[T]): H

    trait Builder {
      def value: T

      def buildWith(avoid: List[String], subs: Map[String, Task], args: List[Task], vars: List[String]): H

      def build: H = buildWith(Nil, Map.empty, Nil, Nil)
    }

    class AlsoAvoid(builder: Builder, x: String) extends Builder {
      override def value: T = mask(x, builder.value)

      override def buildWith(avoid: List[String], subs: Map[String, Task], args: List[Task], vars: List[String]): H =
        builder.buildWith(x :: avoid, subs, args, vars)
    }

    trait Task {
      def value: T

      def alsoAvoid(x: String): Task
    }

    case class NonTerminal(builder: Builder, avoid: List[String], subs: Map[String, Task]) extends Task {
      def value: T = T.where(builder.value, subs.mapValues(_.value))

      def alsoAvoid(x: String): Task =
        NonTerminal(builder, x :: avoid, subs.mapValues(_.alsoAvoid(x)))
    }

    case class Terminal(n: String, m: Int) extends Task {
      override def value: T = mask(n, m)

      override def alsoAvoid(x: String): Task = if (n == x) copy(n = n + 1) else this
    }


    //the hard part is deciding to stop evaluating at a late stage
    object Reduce extends Term[Builder] {
      override def identifier(name: String): Builder = new Builder {
        override def value: T = T.identifier(name)

        override def buildWith(avoid: List[String], subs: Map[String, Task], args: List[Task], vars: List[String]): H =
          subs.get(name) match {
            case None => result(vars.reverse, name, avoid.count(_ == name), args.map(_.value))
            case Some(Terminal(x, y)) => result(vars.reverse, x, y + avoid.count(_ == x), args.map(_.value))
            case Some(NonTerminal(builder, avoid2, subs2)) =>
              builder.buildWith(avoid2 ++ avoid, subs ++ subs2, args, vars)
          }
      }

      override def where(term: Builder, substitution: Map[String, Builder]): Builder = new Builder {
        override def value: T = T.where(term.value, substitution.mapValues(_.value))

        override def buildWith(avoid: List[String], subs: Map[String, Task], args: List[Task], vars: List[String]): H =
          term.buildWith(
            avoid.filterNot(subs.contains),
            subs ++ substitution.mapValues(NonTerminal(_, avoid, Map.empty)), args, vars)
      }

      override def application(operator: Builder, operands: List[Builder]): Builder = new Builder {
        override def value: T = T.application(operator.value, operands.map(_.value))

        override def buildWith(avoid: List[String], subs: Map[String, Task], args: List[Task], vars: List[String]): H =
          operator(avoid, subs, operands.map(NonTerminal(_, avoid, subs)) ++ args, vars)
      }

      override def abstraction(arg: String, body: Builder): Builder = new Builder {
        override def value: T = T.abstraction(arg, body.value)

        override def buildWith(avoid: List[String], subs: Map[String, Task], args: List[Task], vars: List[String]): H = args match {
          case Nil =>
            val stringToTask = (subs + (arg -> subs.getOrElse(arg, Terminal(arg, 0)))).mapValues(_.alsoAvoid(arg))
            body.buildWith(avoid, stringToTask, Nil, arg :: vars)
          case h :: t => body.buildWith(avoid,subs + (arg -> h), t, vars)
        }
      }
    }

  }

}
