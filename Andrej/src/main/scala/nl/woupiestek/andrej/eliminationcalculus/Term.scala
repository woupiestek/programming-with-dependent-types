package nl.woupiestek.andrej.eliminationcalculus

import scala.language.postfixOps

sealed trait Term {
  def reduce(environment: List[Declaration]): Declaration
}

case class Axiom(key: Int) extends Term {
  override def reduce(environment: List[Declaration]): Declaration = environment lift key match {
    case None => Declaration(Nil, this)
    case Some(Declaration(is, o)) =>
      val result = o.reduce(environment.drop(key))
      result.copy(inputs = is ++ result.inputs)
  }
}

case class ArrowRight(operator: Term, operands: List[Term]) extends Term {
  override def reduce(environment: List[Declaration]): Declaration = {
    val retor = operator.reduce(environment)
    val rends = operands.map(term(_, environment))
    retor.appliedTo(rends)
  }

  private def term(operand: Term, environment: List[Declaration]): Term =
    ArrowLeft(operand.reduce(environment), Axiom(0))

}

case class Declaration(inputs: List[Term], output: Term) {
  def appliedTo(operands: List[Term]): Declaration = {
    if (inputs.length > operands.length) {
      val (head, tail) = inputs.splitAt(operands.length)
      val reduct = output.reduce(environment(head zip operands))
      reduct.copy(inputs = tail ++ reduct.inputs)
    } else {
      val (head, tail) = operands.splitAt(inputs.length)
      output.reduce(environment(inputs zip head)).appliedTo(tail)
    }
  }

  private def environment(pairs: List[(Term, Term)]): List[Declaration] =
    pairs map { case (_, t) => Declaration(Nil, t) } reverse

}

case class ArrowLeft(declaration: Declaration, continuation: Term) extends Term {
  override def reduce(environment: List[Declaration]) =
    continuation.reduce(declaration :: environment)
}

