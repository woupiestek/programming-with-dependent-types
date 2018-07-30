package nl.woupiestek.andrej.equalambda

import nl.woupiestek.andrej.equalambda.HNF._
import nl.woupiestek.andrej.free.TagFree
import nl.woupiestek.andrej.free.TagFree._

import scala.language.reflectiveCalls

case class HNF[V](vars: List[V], head: V, args: List[HNF[V]]) {

  def replace(f: Map[V, V]): HNF[V] =
    HNF(vars, f.getOrElse(head, head), args.map(_.replace(f.filterKeys(!vars.contains(_)))))

  def applyTo(as: List[V]): HNF[V] = if (vars.length >= as.length) {
    val (v0, v1) = vars.splitAt(as.length)
    copy(vars = v1).replace(v0.zip(as).toMap)
  } else {
    val (a0, a1) = as.splitAt(vars.length)
    val hnf2 = copy(vars = Nil).replace(vars.zip(a0).toMap)
    hnf2.copy(args = hnf2.args ++ a1.map(lift))
  }

  def etaExpand(n: Int): M[V, HNF[V]] =
    List.fill(n)(fresh[V]).sequence.map(vs => copy(vars = vars ++ vs, args = args ++ vars.map(lift)))
}

object HNF {

  def lift[V](v: V) = HNF(Nil, v, Nil)

  trait Effect[V, W]

  case class Fresh[V]() extends Effect[V, V]

  type M[V, W] = TagFree[({type E[U] = Effect[V, _]})#E, W]

  def fresh[V]: M[V, V] = TagFree.execute[({type E[U] = Effect[V, _]})#E, V](Fresh())

  case class Judgement[V](vars: List[V], antes: Set[(HNF[V], HNF[V])], left: HNF[V], right: HNF[V]) {
    def matchRule: Set[M[V, Judgement[V]]] = {
      def helper(a: HNF[V], b: HNF[V], c: HNF[V], d: HNF[V]): M[V, Judgement[V]] = {
        def helper2(b2: HNF[V], c2: HNF[V]) =
          Judgement(vars ++ b2.vars ++ b2.vars, antes ++ b2.args.zip(c2.args), a.applyTo(b2.vars), d.applyTo(c2.vars))

        b.args.length - c.args.length match {
          case x if x > 0 => c.etaExpand(x).map(helper2(b, _))
          case 0 => TagFree.pure(helper2(b, c))
          case x if x < 0 => b.etaExpand(x).map(helper2(_, c))
        }
      }

      antes.collect {
        case (l, r) if l.head == left.head => helper(r, l, left, right)
        case (l, r) if r.head == left.head => helper(l, r, left, right)
      } ++ antes.collect {
        case (l, r) if l.head == right.head => helper(r, l, right, left)
        case (l, r) if r.head == right.head => helper(l, r, right, left)
      }
    }

    /* more rules...
    *
    *  NB bound variables cannot match
    *
    *  1. for same head left & right
    *  2. matching heads in different pairs in ante
    *  3. matching head left in right in ante
    *  4. elimination of vars
    *
    *  arities have to match up or simple typing must fail
    *  in the antes it is an automatic success--otherwise automatic fail
    *  if they do match up, things get complicated
    *
    * */

  }


}
