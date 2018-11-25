package nl.woupiestek.equalizer

object Form2 {

  sealed trait Term

  case class Index(depth: Int) extends Term

  case class Substitution(
    typ: Type, value: Term, body: Term) extends Term

  case class Abstraction(typ: Type, body: Term) extends Term

  case class Application(
    operator: Term, operands: List[Term]) extends Term

  case class Reflection(left: Term, right: Term) extends Term


  sealed trait Type

  case class TypeIndex(depth: Int) extends Type

  case class Product(typ: Type, body: Type) extends Type

  case class Path(left: Term, right: Term) extends Type

  case class SubstitutionType(
    typ: Type, value: Term, body: Type) extends Type


  sealed trait Proposition

  case class Typing(term: Term, typ: Type) extends Proposition

  case class Equation(left: Term, right: Term) extends Proposition

  case class TypeEquation(left: Type, right: Type) extends Proposition

  case object False extends Proposition

  case class Clause(prop: Proposition, types: List[Type]) {

    def next: Set[Clause] = prop match {
      case Typing(Abstraction(t, b), Product(t2, b2)) =>
        Set(Clause(TypeEquation(t, t2), types),
          Clause(Typing(b, b2), t :: types))
      case Typing(Reflection(a, b), Path(c, d)) =>
        Set(Equation(a, b), Equation(a, c), Equation(b, d))
          .map(Clause(_, types))
      case Typing(Substitution(t, v, b), t2) =>
        Set(Typing(v, t), Typing(b, t2)) //missing substitution...
          .map(Clause(_, types))
      case Typing(Application(a, b), c) => b match {
        case Nil => Set(Clause(Typing(a, c), types))
        case b0 :: b1 => a match {
          case Abstraction(d,e) => Set(
            Clause(Typing(b0,d),types),
            Clause(Typing(Application(e,b1),c),d::types)) //missing substitution
          case Reflection(d,e) => Set(False)
          case Substitution(d,e,f) =>



            ???
        }
      }
        ???
      //missing clause for application
      case Equation(Abstraction(t, b), t2) =>
        Set(Equation(b, Application(t2, Index(0) :: Nil)))
          .map(Clause(_, t :: types))
      case Equation(Reflection(a, b), Reflection(c, d)) =>
        Set(Equation(a, b), Equation(a, c), Equation(b, d))
          .map(Clause(_, types))
      case Equation(Substitution(t, v, b), t2) =>
        Set(Typing(v, t), Equation(b, t2)) //missing substitution...
          .map(Clause(_, types))
      //missing clause for application
      //nothing on generalizations
    }
  }

  case class Task(
    term: Term,
    subs: List[Task],
    args: List[Task]) {

    def substitute(subs2: List[Task]): Task =
      Task(term, subs ++ subs2, args.map(_.substitute(subs2))) //ai!

    def apply(args2: List[Task]): Task = copy(args = args ++ args2)

    def next: Task = term match {
      case Index(i) =>
        if (args.length > i)
          subs(i).substitute(subs.drop(i)).apply(args)
        else this
      case Substitution(_, v, b) => Task(b, Task(v, subs, Nil) :: subs, args) //annotate with proposition?
      case Abstraction(_, body) => args match { //annotate with proposition?
        case h :: t => Task(body, h :: subs, t)
        case Nil => this
      }
      case Application(a, b) => Task(a, subs, Task(b, subs, Nil) :: args)
      case Reflection(a, b) => this
    }
  }

}