package nl.woupiestek.andrej.yetanothersystem

trait SbStatement {
  def push(index: Int, value: SbExpression, expression: SbExpression): SbExpression

  def handle(arg: SbExpression, expression: SbExpression): SbExpression
}

case class ArL(index: Int, arg: SbExpression) extends SbStatement {
  override def push(i: Int, value: SbExpression, expression: SbExpression): SbExpression =
    if (i == index) expression.push(0, value.handle(arg))
    else if (i > index) expression.push(i + 1, value) //???
    else expression.push(i, value) //???

  override def handle(arg: SbExpression, expression: SbExpression): SbExpression =
    Sequence(this, expression.handle(arg))
}

object ArR extends SbStatement {
  override def push(index: Int, value: SbExpression, expression: SbExpression): SbExpression =
    Sequence(this, expression.push(index + 1, value))

  override def handle(arg: SbExpression, expression: SbExpression): SbExpression =
    expression.push(0, arg)
}

trait SbExpression {
  def push(index: Int, value: SbExpression): SbExpression

  def handle(arg: SbExpression): SbExpression
}

case class Return(index: Int) extends SbExpression {
  override def push(i: Int, value: SbExpression): SbExpression = {
    if (index == i) value
    else if (index > i) Return(index - 1)
    else this
  }

  override def handle(arg: SbExpression): SbExpression = Sequence(ArL(index, arg), Return(0))
}

case class Sequence(statement: SbStatement, expression: SbExpression) extends SbExpression {
  override def push(index: Int, value: SbExpression): SbExpression = statement.push(index, value, expression)

  override def handle(arg: SbExpression): SbExpression = statement.handle(arg, expression)
}

