package nl.woupiestek.equalizer.game

import Edge._
sealed trait Edge
object Edge {
  case class Argument(identifier: String) extends Edge
  case object Operator extends Edge
  case object Operand extends Edge
}

case class ATerm(get: List[Edge] => Option[String])
    extends AnyVal

object ATerm {
  def variable(name: String) = ATerm {
    case Nil => Some(name)
    case _   => None
  }
  def application(operator: ATerm, operand: ATerm) = ATerm {
    case Operator :: t => operator.get(t)
    case Operand :: t  => operand.get(t)
    case _             => None
  }
  def abstraction(arg: String, body: ATerm) = ATerm {
    case Argument(identifier) :: t if identifier == arg =>
      body.get(t)
    case _ => None
  }

  //intensive...
  def let(key: String, value: ATerm, context: ATerm): ATerm =
    if (context.get(Nil) == Some(key)) value
    else
      ATerm {
        case h :: t if h != Argument(key) =>
          let(key, value, ATerm(t2 => context.get(h :: t2)))
            .get(t)
        case other => context.get(other)
      }
}

case class BTerm(vars: Map[List[Edge], String]) extends AnyVal

object BTerm {
  def variable(name: String) = BTerm(Map(Nil -> name))
  def application(operator: BTerm, operand: BTerm) =
    BTerm(
      operator.vars.map {
        case (key, value) => (Operator :: key) -> value
      } ++
        operand.vars.map {
          case (key, value) => (Operand :: key) -> value
        }
    )

  def abstraction(arg: String, body: BTerm) = BTerm(
    body.vars.map {
      case (key, value) => (Argument(arg) :: key) -> value
    }
  )

  def let(key: String, value: BTerm, context: BTerm) =
    BTerm(context.vars.flatMap {
      case (edges, identifier)
          if key == identifier && !edges
            .contains(Argument(key)) =>
        value.vars.map { case (k, v) => (edges ++ k) -> v }
      case other => Set(other)
    })
}
