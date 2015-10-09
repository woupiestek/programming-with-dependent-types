package nl.woupiestek.andrej.test

import nl.woupiestek.andrej._
import org.scalatest._

import scala.util.Success

class ParseTest extends FlatSpec {


  def checkParboil(input: String, output: Expr[String]) {
    assert(Parboil(input) == Success(output))
  }

  "Parboil" should "recognize variables" in checkParboil("x", Iden("x"))
  it should "recognize universes" in checkParboil("Type", Type)
  it should "recognize abstraction" in checkParboil("\\x:y.z", Abst(Iden("y"), Iden(Some("z"))))
  it should "recognize products" in checkParboil("Prod x:y.z", Prod(Abst(Iden("y"), Iden(Some("z")))))
  it should "recognize applications" in checkParboil("(x y z)", Appl(Iden("x"), List(Iden("y"), Iden("z"))))


}


