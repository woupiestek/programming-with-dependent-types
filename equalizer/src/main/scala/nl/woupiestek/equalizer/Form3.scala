package nl.woupiestek.equalizer

object Form3 {

  sealed trait Term

  case class Variable(name: String) extends Term

  case class Abstraction(key: String, typ: Type, body: Term) extends Term

  case class Application(operator: Term, operand: Term) extends Term

  case class Reflection(left: Term, right: Term) extends Term

  sealed trait Type

  case class TypeVariable(name: String) extends Type

  case class Product(key: String, typ: Type, body: Type) extends Type

  case class Path(left: Term, right: Term) extends Type

  case class Let(name: String, type1: Type, value: Term, type0: Type) extends Type


  case class Clause(
    context: List[(String, Type)],
    operator: Term,
    operands: List[Term],
    typ: Type) {


    //not quite good enough...
    //use actual patterns...
    def next:Set[Clause] = (operator, operands) match {
      case (Reflection(a, b), Nil) => typ match {
          case Path(a2,b2) if a==a2 && b==b2 => Set(this)
          case _ => ???
        }
      case (Reflection(a, b), c :: d) => ???
      case (Application(a, b), c) => Set(Clause(context, a, b :: c, typ))
      case (Abstraction(a, b, c), Nil) =>
        Set(Clause((a, b) :: context, c, Nil, typ))
      case (Abstraction(a, b, c), d :: e) => typ match {
        case Let(a2, b2, d2, f) if a2 == a && b2 == b && d2 == d => Set(
          Clause(context, d, Nil, b),
          Clause((a, b) :: context, c, e, f))
        case _ => ???
      }
      case (Variable(a),Nil) => context.dropWhile { case (k, v) => k != a } match {
        case Nil => ???
        case (_, h) :: t if h==typ => Set.empty
      }
    }

  }

}