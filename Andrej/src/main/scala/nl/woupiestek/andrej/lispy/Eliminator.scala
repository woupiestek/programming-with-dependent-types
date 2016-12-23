package nl.woupiestek.andrej.lispy

object Eliminator {

  def eliminate(expr: Expr, context: Map[String, Expr]): Option[Expr] = expr match {
    case Var(x) => context.get(x)
    case Let(Var(x), y, z) => for {
      y2 <- eliminate(y, context)
      z2 <- eliminate(z, context + (x -> y2))
    } yield z2
    case Product(entries) =>
      val es = for {
        (index, expr) <- entries
        e <- eliminate(expr, context)
      } yield index -> e
      Some(Product(es))
    case Select(Var(x), y, z) => context.get(x).flatMap {
      case Product(entries) => for {
        y2 <- entries.get(y)
        z2 <- eliminate(z, context + (x -> y2))
      } yield z2
      case _ => eliminate(z, context).map(z2 => Select(Var(x), y, z2))
    }
    case Lambda(Var(x), y) => eliminate(y, context - x).map(z => Lambda(Var(x), z)) //risk of variable capture!
    case Apply(Var(x), y, z) => context.get(x).flatMap {
      case Lambda(Var(a), b) => for {
        y2 <- eliminate(y, context)
        b2 <- eliminate(b, Map(a -> y2))
        z2 <- eliminate(z, context + (x -> b2))
      } yield z2
      case _ => for {
        y2 <- eliminate(y, context)
        z2 <- eliminate(z, context - x)
      } yield Apply(Var(x), y2, z2)
    }
  }

  /** Non strict variant **/
  case class Task(expr: Expr, context: Map[String, Task]) {
    def next: Option[Task] = expr match {
      case Var(x) => context.get(x)
      case Let(Var(x), y, z) => Some(Task(z, context + (x -> Task(y, context))))
      case Product(_) => None
      case Select(Var(x), y, z) => context.get(x).flatMap {
        case Task(Product(entries), context2) => entries.get(y).map {
          y2 => Task(z, context + (x -> Task(y2, context2)))
        }
        case task => task.next.map(x2 => Task(expr, context + (x -> x2)))
      }
      case Lambda(_, _) => None
      case Apply(Var(x), y, z) => context.get(x).flatMap {
        case Task(Lambda(Var(a), b), context2) =>
          val y2 = Task(y, context)
          val b2 = Task(b, context2 + (a -> y2))
          Some(Task(z, context + (x -> b2)))
        case task => task.next.map(x2 => Task(expr, context + (x -> x2)))
      }
    }
  }

}
