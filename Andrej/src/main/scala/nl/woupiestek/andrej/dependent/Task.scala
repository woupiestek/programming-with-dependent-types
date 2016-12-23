package nl.woupiestek.andrej.dependent

import scala.annotation.tailrec

sealed trait Task {
  def next: Option[Task]

  @tailrec final def eval: Task = next match {
    case None => this
    case Some(x) => x.eval
  }
}

case class PushTask(value: Expression, stack: List[Task]) extends Task {
  override def next: Option[Task] = value match {
    case Get(i) => stack lift i
    case Push(x, y) => Some(PushTask(y, PushTask(x, stack) :: stack))
    case Function(_, _) => None //value?
    case Apply(x, y, z) => for {
      a <- stack lift x
    } yield {
      val t = PushTask(y, stack)
      val u = ApplyTask(a, t)
      PushTask(z, u :: stack)
    }
    case Product(_, _) => None //value?
    case Omega => None //value?
  }
}

case class ApplyTask(x: Task, y: Task) extends Task {
  override def next: Option[Task] = x match {
    case PushTask(Function(_, a), b) => Some(PushTask(a, y :: b))
    case _ => x.next.map(ApplyTask(_, y))
  }
}


//I don't trust this part
case class TypeTask(value: Expression, stack: List[Task]) extends Task {
  override def next: Option[Task] = value match {
    case Get(i) => stack lift i
    case Push(x, y) => Some(TypeTask(y, TypeTask(x, stack) :: stack))
    case Function(a, b) => Some(TypeTask(b, PushTask(a, stack) :: stack)) //sticking with the 'no product' idea!
    case Product(a, b) => Some(PushTask(b, PushTask(a, stack) :: stack))
    case Omega => None
    case Apply(x, y, z) =>
      for {
        tx <- stack lift x
      } yield {
        val ty = TypeTask(y, stack)
        val txy = InferenceTask(tx, ty)
        TypeTask(z, txy :: stack)
      }
  }
}

case class InferenceTask(x: Task, y: Task) extends Task {
  //all is here...
  private def equal(ta: Task, ty: Task) = (ta.eval, ty.eval) match {
    case (PushTask(a, b), PushTask(c, d))
      if a.getClass == b.getClass && a.ind == c.ind && a.subs.length == c.subs.length =>
      val tas = a.subs.map(PushTask(_, b))
      val tcs = c.subs.map(PushTask(_, d))
      Some(tas.zip(tcs))
    case _ => None
  }

  private def all(list: List[(Task, Task)]): Boolean = list match {
    case Nil => true
    case (a, b) :: tail => equal(a, b) match {
      case None => false
      case Some(list2) => all(list2 ++ list)
    }
  }

  override def next: Option[Task] = x match {
    case PushTask(Product(b, a), c) => if (equal(y, PushTask(b, c)).exists(all)) Some(PushTask(a, c)) else None
    case _ => x.next.map(InferenceTask(_, y))
  }

}
