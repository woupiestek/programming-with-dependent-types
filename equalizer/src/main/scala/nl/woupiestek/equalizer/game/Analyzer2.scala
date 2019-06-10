package nl.woupiestek.equalizer.game

object Analyzer2 {

  final case class Form(
      left: Term,
      right: Term,
      tail: List[Either[Sentence, String]]
  )

  @annotation.tailrec
  def analyze(
      sentence: Sentence,
      stack: List[Either[Sentence, String]]
  ): Form =
    sentence match {
      case Equation(left, right) => Form(left, right, stack)
      case Generalization(varName, body) =>
        analyze(body, Right(varName) :: stack)
      case Implication(ante, con) =>
        analyze(con, Left(ante) :: stack)
    }
}
