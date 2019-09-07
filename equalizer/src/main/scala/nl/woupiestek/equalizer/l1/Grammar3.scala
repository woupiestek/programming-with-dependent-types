package nl.woupiestek.equalizer.l1

//import scalaz._
//import Scalaz._
import nl.woupiestek.equalizer.parsing.Input

class Grammar3[D](
    //D: AST.Def[D]
) {

  type Q[A] = Input[Char] => (A, Input[Char])

  def whitespace(input: Input[Char]): Boolean =
    Character.isWhitespace(input.head)
    
  def identifier(
      input: Input[Char]
  ): Option[String] = {

    def iPart(input: Input[Char]): Option[List[Char]] =
      if (Character.isJavaIdentifierPart(input.head))
        Some(input.head :: iPart(input.tail).getOrElse(Nil))
      else
        None

    if (Character.isJavaIdentifierStart(input.head))
      iPart(input.tail)
        .map(chars => (input.head :: chars).mkString)
    else
      None
  }

  def integer(input: Input[Char]): Option[Int] = {
    def digits(input: Input[Char]): Option[List[Int]] =
      if ('0' <= input.head && input.head <= '9')
        Some(
          (input.head - '0') :: digits(input.tail)
            .getOrElse(Nil)
        )
      else
        None

    digits(input).map(
      ds => (ds.foldLeft(0) { case (j, k) => 10 * j + k })
    )
  }
/*
  def expression(input: Input[Char]): Input[Option[D]] = {

    def unit: Option[D] = identifier(input).map(D.variable).orElse(
      if(input.head == '(') whitespace(input.tail)
    )

    def elims: D = ???

    def intros: D = ???

    intros
  }

  val defExp: Q[D] = {

    def iOp(a: String, expression: Q[D]): Q[D] = {
      (for {
        _: Unit <- token('=')
        b: D <- expression
        _: Unit <- token(';')
        c: D <- expression
      } yield D.let(a, b, c)) ++
        (for {
          _: Unit <- token("->")
          b: D <- expression
        } yield D.abstraction(a, b)) ++
        (for {
          _: Unit <- token('@')
          b: D <- expression
        } yield D.fix(a, b))
    }

    def dOp(d: D, expression: Q[D]): Q[D] = {
      Parser3.point(d) ++
        (token('\'').map((_: Unit) => D.unfold(d)) ++
          (for {
            i: Int <- integer
            _: Unit <- whitespace
          } yield D.project(d, i)) ++
          expression.map(D.application(d, _)))
          .flatMap(dOp(_, expression))
    }

    def tOp(expression: Q[D]): Q[List[D]] =
      expression.flatMap(
        (h: D) =>
          token('>').map((_: Unit) => List(h)) ++
            (token(',') *>
              tOp(expression).map(h :: (_: List[D])))
      )

    Parser3.loop { (expression: Q[D]) =>
      identifier.flatMap(
        (s: String) =>
          dOp(D.variable(s), expression) ++ iOp(
            s,
            expression
          )
      ) ++ (for {
        _: Unit <- token('(') //
        d: D <- expression
        _: Unit <- token(')')
      } yield d) ++ (for {
        _: Unit <- token('<')
        ds: List[D] <- (tOp(expression) ++
          token('>').map((_: Unit) => List.empty[D]))
      } yield D.tuple(ds))
    }
  }*/
}
