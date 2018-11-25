package nl.woupiestek.equalizer

import scala.language.higherKinds

class Form4 {

  sealed trait Term[E, Y]

  case class EVar[E, Y](name: String) extends Term[E, Y]

  case class EAppl[E, Y](operator: E, operands: List[E]) extends Term[E, Y]

  case class EAbst[E, Y](evar: String, dom: Type[E, Y], body: Term[E, Y]) extends Term[E, Y]

  case class ERefl[E, Y](left: E, right: E) extends Term[E, Y]

  sealed trait Type[E, Y]

  case class YVar[E, Y](name: String) extends Type[E, Y]

  case class YProd[E, Y](evar: String, dom: Type[E, Y], cod: Type[E, Y]) extends Type[E, Y]

  case class YPath[E, Y](left: E, right: E) extends Type[E, Y]

  case class YLet[E, Y](evar: String, dom: Type[E, Y], value: Term[E, Y], cod: Type[E, Y]) extends Type[E, Y]


  //add more?
  sealed trait Free[F[_], X] {
    def flatMap[Y](f: X => Free[F, Y]): Free[F, Y]

    def map[Y](f: X => Y): Free[F, Y] = flatMap((x: X) => Pure(f(x)))
  }

  case class Pure[F[_], X](x: X) extends Free[F, X] {
    override def flatMap[Y](f: X => Free[F, Y]): Free[F, Y] = f(x)
  }

  case class Bind[F[_], X, Y](fx: F[X], f: X => Free[F, Y]) extends Free[F, Y] {
    override def flatMap[Z](g: Y => Free[F, Z]): Free[F, Z] =
      Bind(fx, f(_: X).flatMap(g))
  }

  //oh dear
  type FE = Free[Term[_, FY], Int]
  type FY = Free[Type[FE, _], Int]




}
