package nl.woupiestek.andrej.equalambda.attempt2

trait HeadNormalForm[H, T] {
  def result(arity: Int, index: Int, args: List[T]): H

  def result(arity: Int, name: String, args: List[T]): H

  type Tasks = Map[String, Task]
  val emptyTasks: Tasks = Map.empty

  def where(t: T, tasks: Tasks)(implicit T: Term[T]): T = T.where(t, tasks.mapValues(_.value))

  def comp(f: Tasks, g: Tasks): Tasks = g ++ f.mapValues(_.compose(g))

  trait Builder {
    def value: T

    def buildWith(subs: Tasks, args: List[Task], index: Int): H

    def build: H = buildWith(emptyTasks, Nil, 0)
  }

  class Index(int: Int)(implicit T: Int => T) extends Builder {
    override def value: T = T(int)

    override def buildWith(subs: Tasks, args: List[Task], index: Int): H = result(index + 1, int, args.map(_.value))
  }

  case class Task(builder: Builder, subs: Tasks)(implicit T: Term[T]) {
    def value: T = where(builder.value, subs)

    def continueWith(args: List[Task], index: Int): H = builder.buildWith(subs, args, index)

    def compose(sigma: Map[String, Task]): Task = copy(subs = comp(subs, sigma))
  }

  def reduce(implicit T: Term[T], param: Int => T): Term[Builder] = new Term[Builder] {
    override def identifier(name: String): Builder = new Builder {
      override def value: T = T.identifier(name)

      override def buildWith(subs: Tasks, args: List[Task], index: Int): H =
        subs.get(name).map(_.continueWith(args, index)).getOrElse(result(index + 1, name, args.map(_.value)))
    }


    override def where(term: Builder, substitution: Map[String, Builder]): Builder = new Builder {
      override def value: T = T.where(term.value, substitution.mapValues(_.value))

      override def buildWith(subs: Tasks, args: List[Task], index: Int): H =
        term.buildWith(comp(substitution.mapValues(Task(_, emptyTasks)), subs), args, index)
    }

    override def application(operator: Builder, operands: List[Builder]): Builder = new Builder {
      override def value: T = T.application(operator.value, operands.map(_.value))

      override def buildWith(subs: Tasks, args: List[Task], index: Int): H =
        operator.buildWith(subs, operands.map(Task(_, subs)) ++ args, index)
    }

    override def abstraction(arg: String, body: Builder): Builder = new Builder {
      override def value: T = T.abstraction(arg, body.value)

      override def buildWith(subs: Tasks, args: List[Task], index: Int): H = args match {
        case Nil => body.buildWith(comp(subs, Map(arg -> Task(new Index(index), emptyTasks))), Nil, index + 1)
        case h :: t => body.buildWith(subs + (arg -> h), t, index)
      }
    }
  }

}

