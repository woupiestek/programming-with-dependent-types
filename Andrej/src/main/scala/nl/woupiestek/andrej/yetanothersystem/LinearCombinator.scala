package nl.woupiestek.andrej.yetanothersystem

trait LinearCombinator[T] {
  def pop: T

  def push(arg: T, cont: T): T

  def lambda(arg: T): T

  def apply(operator: T, operand: T): T
}

object LinearCombinator {

  case class L(run: L => List[L])

  type M = List[L] => List[L]

  val instance = new LinearCombinator[M] {
    override def pop: M = x => x

    override def push(arg: M, cont: M): M =
      list => cont(arg(list))

    override def lambda(arg: M): M =
      tail => L(head => arg(head :: tail)) :: Nil

    override def apply(operator: M, operand: M): M =
      list =>
        operator(list) match {
          case L(f) :: t1 =>
            operand(t1) match {
              case x :: t2 => f(x) ++ t2
              case _       => Nil
            }
          case _ => Nil
        }
  }
}
