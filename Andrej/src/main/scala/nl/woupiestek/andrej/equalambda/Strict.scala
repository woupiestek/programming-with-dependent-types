package nl.woupiestek.andrej.equalambda

import nl.woupiestek.andrej.free.TagFree
import nl.woupiestek.andrej.free.TagFree._

object Strict {

  //variabelen verpesten alles.
  type Variable = Either[String, Int]

  def named(name: String): Variable = Left(name)

  sealed trait Value extends Builder

  case class Appl(head: Variable, args: List[Value]) extends Value {
    override def buildWith(operands: List[Value], vars: List[Variable]): TagFree[Effect, Value] =
      execute(Get(head)).flatMap {
        case Some(w) => w.buildWith(args ++ operands, vars)
        case None => pure(vars.foldLeft[Value](Appl(head, operands))((v0, v1) => Abst(v1, v0)))
      }
  }

  case class Abst(head: Variable, body: Value) extends Value {
    override def buildWith(operands: List[Value], vars: List[Variable]): TagFree[Effect, Value] =
      operands match {
        case Nil => body.buildWith(Nil, head :: vars)
        case h :: t => execute(Put(head, h)).flatMap(_ => body.buildWith(t, vars))
      }
  }

  def lift(variable: Variable): Value = Appl(variable, Nil)

  sealed trait Effect[X]

  case object Fresh extends Effect[Variable]

  case class Put(key: Variable, value: Value) extends Effect[Unit]

  case class Get(key: Variable) extends Effect[Option[Value]]

  case class Unify(left: Value, right: Value) extends Effect[Unit]

  //this is a bit underwhelming...
  case class Test(left: Value, right: Value) extends Effect[Boolean]

  case object Fail extends Effect[Value]

  trait Builder {
    def buildWith(operands: List[Value], vars: List[Variable]): TagFree[Effect, Value]

    def build(): TagFree[Effect, Value] = buildWith(Nil, Nil)
  }

  def reduce(): TermWithQ[Builder] = new TermWithQ[Builder] {
    override def check(left: Builder, right: Builder, term: Builder): Builder = {
      (operands: List[Value], vars: List[Variable]) =>
        for {
          l <- left.build()
          r <- right.build()
          b <- execute(Test(l, r))
          t <- if (b) term.buildWith(operands, vars) else execute(Fail)
        } yield t
    }

    override def unify(left: Builder, right: Builder, term: Builder): Builder =
      (operands: List[Value], vars: List[Variable]) => for {
        l <- left.build()
        r <- right.build()
        _ <- execute(Unify(l, r))
        t <- term.buildWith(operands, vars)
      } yield t


    override def identifier(name: String): Builder = lift(named(name))

    override def where(term: Builder, substitution: Map[String, Builder]): Builder =
      (operands: List[Value], vars: List[Variable]) => for {
        _ <- substitution.traverse {
          case (key, builder) => for {
            value <- builder.build()
            _ <- execute(Put(named(key), value))
          } yield ()
        }
        t <- term.buildWith(operands, vars)
      } yield t

    override def application(operator: Builder, operands: List[Builder]): Builder =
      (operands2: List[Value], vars: List[Variable]) => for {
        ys <- operands.traverse(_.build())
        x <- operator.buildWith(ys ++ operands2, vars)
      } yield x

    override def abstraction(arg: String, body: Builder): Builder =
      (operands: List[Value], vars: List[Variable]) => operands match {
        case Nil => for {
          f <- execute(Fresh)
          _ <- execute(Put(named(arg), lift(f)))
          b <- body.buildWith(operands, f :: vars)
        } yield b
        case h :: t => for {
          _ <- execute(Put(named(arg), h))
          b <- body.buildWith(t, vars)
        } yield b
      }
  }

  /*
  * # hogere orde unificatie
  *
  * aangenomen is dat alle vergelijkinge gereduceerd zijn tot de vorm \a_.bC_ = \d_.eF_
  * als we op zoek zijn naar een tegenvoorbeeld kunnen we nog verder gaan en alles in de vorm
  * bG_ = eH_ krijgen
  *
  * Nu willen we echter een soort hogere order pattern matching doen. i.a.w.
  * Gegeven \a_.bC_ = D ... in dit geval mag de vergelijking dus voor alle waardes van a_i geldig zijn.
  * We zien bE_ = F staan.
  * Nu maken we nieuwe aannames C_i = B_i (voor alle a_j) en proberen dan vast te stellen of
  * D = Fa_ onder die aannames.
  * Ik ben nu slordig met ariteit, maar we passen in principe alles aan het 'patroon' aan, dat is leidend
  * In deze stap elimineren we de variabele 'b' schijnbaar aan beide kanten. Daardoor nemen we het risico
  *
  * Vergelijking zijn symmetrisch, dus op elk punt kunnen we twee keuzes maken.
  * Het doel is zo snel mogelijk een beslissing te maken.
  *
  * We hebben in principe steeds een lijst van vergelijkingen aan de linkerkant, en één vergelijking aan de rechterkant
  * Vrije variabelen aan de rechterkant zijn de plaatsen waar de skepticus tegenvoorbeelden in stopt
  * Het delen van vrije variabelen met andere termen is daarom de sleutel tot succes voor de gelovige.
  * Daarom richt de gelovige zich daar juist op.
  *
  * Dit is ongeveer de regel die we gebruiken:
  * B_ = D_ |- C = E
  * -----------------
  * aB_= C |- aD_ = E
  *
  */

}
