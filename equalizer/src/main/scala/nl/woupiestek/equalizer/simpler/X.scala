package nl.woupiestek.equalizer.simpler

object X {
  //todo: get rid of the strings here?
  case class TermEquations(
    equalities: Set[(Int, Int)] = Set.empty,
    variables: Set[(Int, String)] = Set.empty,
    abstractions: Set[(Int, String, Int)] = Set.empty,
    applications: Set[(Int, Int, Int)] = Set.empty,
    lets: Set[(Int, String, Int, Int)] = Set.empty,
    check: Set[(Int, Int, Int, Int)] = Set.empty)

  case class SimpleTypeEquations(
    equalities: Set[(Int, Int)] = Set.empty,
    arrows: Set[(Int, Int, Int)] = Set.empty)

  type T = (Int, Set[(String, Int)]) =>
    Set[(String, Int)]

  //no increase in index in the output,
  //but something may be needed.
  def variable(name: String): T = (index, vars) => vars + ((name, index))


  /* issue: while reducing, variables and equations attached to them
   * must be taken seriously.
   */


  //use a state monad to track state through parsing
}
