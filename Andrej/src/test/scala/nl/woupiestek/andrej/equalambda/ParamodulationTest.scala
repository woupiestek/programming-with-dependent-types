package nl.woupiestek.andrej.equalambda

import nl.woupiestek.andrej.equalambda.Paramodulation._
import nl.woupiestek.andrej.equalambda.ParamodulationTest._
import org.scalatest.FunSpec

import scala.util.Random

class ParamodulationTest extends FunSpec {
  val random = new Random(1029384756L)

  describe("reduce") {

    it("reduces") {
      val testData = (2 to 4).map(i => (generateTerm(random, i), generateTerm(random, i)))

      testData.foreach { case (x, y) =>
        print(s"${tString(x)}=${tString(y)}:-")
        println(fullReduce(Equation(x, Map.empty, y) :: Nil).map(pString).mkString(","))
        succeed
      }
    }

  }


}


object ParamodulationTest {
  def tString: Paramodulation.Term => String = {
    case Pattern(x, y) => (x :: y.map(tString)).mkString("(", " ", ")")
    case Apply(x, y) => (tString(x) :: y.map(tString)).mkString("(", " ", ")")
    case Where(x, y) => tString(x) + y.map { case (key, value) => s"$key=${tString(value)}" }.mkString("[", ",", "]")
    case Lambda(x, y) => s"\\$x{${tString(y)}}"
    case Reflect(x, y) => s"(${tString(x)}=${tString(y)}?)"
    case Unify(x, y, z) => s"(${tString(x)}=${tString(y)}!${tString(z)})"
  }

  def pString: Proposition => String = {
    case Equation(a, b, c) => s"${tString(Where(a, b))}=${tString(c)}"
    case Match(a, b, c, d) => s"${tString(Pattern(a, b))}=${tString(Pattern(c, d))}"
    case False => "false"
  }

  def generateTerm(random: Random, size: Int): Paramodulation.Term = {

    def varName = {
      List.fill(3)(('a' + random.nextInt(1 + 'z' - 'a')).toChar).mkString
    }

    def variable: Paramodulation.Term = {
      Pattern(varName, Nil)
    }

    def list(length: Int) = {
      List.fill(length)(generateTerm(random, size / length))
    }

    if (size < 2) variable else random.nextInt(6) match {
      case 0 =>
        Pattern(varName, list(1 + random.nextInt(3)))
      case 1 => list(1 + random.nextInt(3)) match {
        case Nil => variable
        case h :: t => Apply(h, t)
      }
      case 2 => list(1 + random.nextInt(3)) match {
        case Nil => variable
        case h :: t => Where(h, t.map(varName -> _).toMap)
      }
      case 3 => Lambda(varName, generateTerm(random, size - 1))
      case 4 => Reflect(generateTerm(random, size / 2), generateTerm(random, size / 2))
      case 5 => Unify(generateTerm(random, size / 3), generateTerm(random, size / 3), generateTerm(random, size / 3))
    }

  }

}