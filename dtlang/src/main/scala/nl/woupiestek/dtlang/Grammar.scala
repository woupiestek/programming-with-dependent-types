package nl.woupiestek.dtlang

import Result._

object Grammar {

  def whitespace: Result[(Int => Char), Nothing, Unit] =
    Result((r, i) => {
      var j = i
      while (Character.isWhitespace(r(j))) j += 1
      Some((Right(()), j))
    })

  def token(
      string: String
  ): Result[(Int => Char), Nothing, Unit] =
    Result(
      (r, i) =>
        if (string.zipWithIndex.forall {
              case (c, j) => c == r(i + j)
            }) {
          var k = i + string.length()
          while (Character.isWhitespace(r(k))) k += 1
          Some((Right(()), k))
        } else {
          None
        }
    )

  def identifier: Result[(Int => Char), Nothing, String] =
    Result((r, i) => {
      var j = i
      while (('a' <= r(i) && r(i) <= 'z')
             || ('A' <= r(i) && r(i) <= 'Z')) j += 1
      var k = j
      while (Character.isWhitespace(r(k))) k += 1
      Some((Right((i to j).map(r).mkString), k))
    })

  def expression: Result[(Int => Char), Nothing, Unit] = {
    lazy val cut: Result[(Int => Char), Nothing, Unit] =
      (for {
        _ <- token("[")
        _ <- identifier
        _ <- token("=")
        _ <- cut
        _ <- token("]")
        _ <- cut
      } yield ()) ++ intro

    lazy val intro: Result[(Int => Char), Nothing, Unit] =
      (for {
        _ <- identifier
        _ <- token("->")
        _ <- cut
      } yield ()) ++ elim

    lazy val elim: Result[(Int => Char), Nothing, Unit] =
      for {
        _ <- unit
        _ <- elim ++ unit
      } yield ()

    lazy val unit: Result[(Int => Char), Nothing, Unit] =
      (for {
        _ <- token("(")
        _ <- cut
        _ <- token(")")
      } yield ()) ++ identifier.map(_ => ())

    cut
  }

}
