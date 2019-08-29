package nl.woupiestek.andrej

object Utils {

  def traverse[X, Y](
      list: List[X]
  )(f: X => Option[Y]): Option[List[Y]] = list match {
    case Nil => Some(Nil)
    case h :: t =>
      for {
        h2 <- f(h)
        t2 <- traverse(t)(f)
      } yield h2 :: t2
  }

}
