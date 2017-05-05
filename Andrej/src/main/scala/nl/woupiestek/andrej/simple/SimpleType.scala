package nl.woupiestek.andrej.simple

case class SimpleType(sources: List[SimpleType], target: Int) {
  def ->:(source: SimpleType): SimpleType = copy(sources = source :: sources)

  def parameters: Set[Int] = sources.flatMap(_.parameters).toSet + target

  override def toString: String = if (sources.isEmpty) target.toString else (sources ++ List(target)).mkString("(", " -> ", ")")
}

object SimpleType {

  object % {
    def apply(index: Int): SimpleType = SimpleType(Nil, index)

    def unapply(arg: SimpleType): Option[Int] = Some(arg.target).filter(_ => arg.sources.isEmpty)
  }

  object ->: {
    def unapply(arg: SimpleType): Option[(SimpleType, SimpleType)] = arg.sources match {
      case Nil => None
      case h :: t => Some((h, SimpleType(t, arg.target)))
    }
  }

}
