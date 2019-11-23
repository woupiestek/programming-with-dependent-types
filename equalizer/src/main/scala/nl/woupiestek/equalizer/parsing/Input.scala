package nl.woupiestek.equalizer.parsing

//should become a comonad
//lazy, memoized, etc.
class Input[I](d: Int => I, index: Int) {
  lazy val head: I = d(index)
  lazy val tail: Input[I] = skip(1)
  def extend[J](f: Input[I] => J): Input[J] =
    new Input(i => f(skip(i)), index)
  def map[J](f: I => J): Input[J] =
    new Input(i => f(d(i)), index)
  def skip(amount: Int): Input[I] =
    if (amount > 0) new Input(d, index + amount) else this
}

object Input {
  def constant[I](i: => I): Input[I] = new Input(_ => i, 0)

  def fromString(string: String): Input[Char] =
    new Input(
      (i: Int) =>
        if (i >= 0 && i < string.length) string.charAt(i)
        else (-1).toChar,
      0
    )
}
