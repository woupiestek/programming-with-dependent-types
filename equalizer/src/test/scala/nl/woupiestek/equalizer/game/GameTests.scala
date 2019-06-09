package nl.woupiestek.equalizer.game

import scala.util.Random

import scalaz._
import Scalaz._
import GameTests._
import org.scalatest._

class GameTests extends FunSpec {

  val random = new Random(1234567890L)

  describe("Analyzer3.analyze") {

    it("generates a disjunctive normal form") {

      (0 to 10)
        .map(sentence(_, Nil).run(random))
        .map(Analyzer3.analyze(_).run(0))
        .map(stringify)
        .foreach(println)
    }

  }

  def stringify(result: Set[Set[Analyzer3.Atom]]): String = {
    def forLambda(pattern: Analyzer3.Lambda): String = pattern match {
      case Analyzer3.Pattern(operator, operands) =>
        s"(${operator}${operands.map(forLambda).mkString})"
      case Analyzer3.Name(term, heap) =>
        val h = heap
          .map { case (k, v) => s"$k:${forLambda(v)}" }
          .mkString("[", ",", "]")
        s"($term$h)"
    }
    result
      .map(_.map {
        case Analyzer3.Equals(_, l, r)  => s"${forLambda(l)}==${forLambda(r)}"
        case Analyzer3.Differs(_, l, r) => s"${forLambda(l)}!=${forLambda(r)}"
      }.mkString("&"))
      .mkString("|")
  }
}

object GameTests {

  type Gen[X] = Reader[Random, X]
  def gen[X](f: Random => X): Gen[X] = Reader(f)

  def choose[X](options: List[Gen[X]]): Gen[X] =
    gen(_.nextInt(options.length)).flatMap(options)

  def element[X](options: List[X]): Gen[X] =
    gen(_.nextInt(options.length)).map(options)

  val char = element(('a' to 'z').toList ++ ('A' to 'Z').toList)
  def string(length: Int): Gen[String] =
    List.fill(length)(char).sequence.map(_.mkString)

  def freshVar: Gen[TermVar] = string(3).map(TermVar)

  def term(maxDepth: Int, vars: List[TermVar]): Gen[Term] =
    if (maxDepth > 0)
      choose(
        List(
          freshVar.flatMap(v => element(v :: vars)),
          freshVar.flatMap(
            v =>
              (term(maxDepth - 1, v :: vars) |@| term(
                maxDepth - 1,
                v :: vars
              ))(Let(v.name, _, _))
          ),
          freshVar
            .flatMap(
              v => term(maxDepth - 1, v :: vars).map(Abstraction(v.name, _))
            ),
          (term(maxDepth - 1, vars) |@| term(maxDepth - 1, vars))(
            Application
          )
        )
      )
    else freshVar.flatMap(v => element(v :: vars))

  def sentence(maxDepth: Int, vars: List[TermVar]): Gen[Sentence] =
    if (maxDepth > 0)
      choose(
        List(
          (term(maxDepth - 1, vars) |@| term(maxDepth - 1, vars))(Equation),
          (sentence(maxDepth - 1, vars) |@| sentence(maxDepth - 1, vars))(
            Implication(_, _)
          ),
          freshVar
            .flatMap(
              v =>
                sentence(maxDepth - 1, v :: vars).map(Generalization(v.name, _))
            )
        )
      )
    else
      freshVar.flatMap(
        u =>
          element(u :: vars)
            .flatMap(v => element(u :: vars).map(Equation(v, _)))
      )

}
