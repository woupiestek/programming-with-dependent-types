package nl.woupiestek.equalizer.parsing

//should become a comonad
//lazy, memoized, etc.
class Input[I](h: => I, t: => Input[I]) {
  lazy val head: I = h
  lazy val tail: Input[I] = t
  def extend[J](f: Input[I] => J): Input[J] =
    new Input(f(this), tail.extend(f))
  def map[J](f: I => J): Input[J] =
    new Input(f(h), t.map(f))
  def skip(amount: Int): Input[I] =
    if (amount > 0) tail.skip(amount - 1) else this
}

object Input {
  def constant[I](i: => I): Input[I] = {
    lazy val j: Input[I] = new Input(i, j)
    j
  }

  def fromString(string: String): Input[Char] = {
    val eof = (-1).toChar
    string.toList.foldRight(constant(eof))(
      new Input(_, _)
    )
  }
}
