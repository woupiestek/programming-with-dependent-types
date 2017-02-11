package nl.woupiestek.andrej.parser

import nl.woupiestek.andrej.Expr

class DTLGrammar[E](e: Expr[E]) {

  type G[X] = Rule[Option[Char], X]

  private lazy val space = Rule.collect[Option[Char], Unit] { case Some(c) if Character.isWhitespace(c) => () }.zeroOrMore

  private lazy val key: G[String] = for {
    h <- Rule.collect[Option[Char], Char] { case Some(c) if Character.isJavaIdentifierStart(c) => c }
    t <- Rule.collect[Option[Char], Char] { case Some(c) if Character.isJavaIdentifierPart(c) => c }.zeroOrMore
    _ <- space
  } yield (h :: t).mkString

  private lazy val identifier: G[E] = key.map(e.identifier)

  private def matchString(string: String): G[Unit] = Rule.matchList(string.map(Some(_)).toList)

  private def token(string: String): G[Unit] = for {
    _ <- matchString(string)
    _ <- space
  } yield ()

  private def token(char: Char): G[Unit] = for {
    _ <- Rule.collect[Option[Char], Unit] { case Some(x) if x == char => () }
    _ <- space
  } yield ()

  private lazy val let: G[E] = for {
    _ <- token('[')
    x <- key
    _ <- token('=')
    y <- term
    _ <- token(']')
    z <- term
  } yield e.let(x, y, z)

  private lazy val lambda: G[E] = for {
    _ <- token('\\')
    x <- key
    _ <- token(':')
    y <- term
    _ <- token('.')
    z <- term
  } yield e.lambda(x, y, z)

  private lazy val universe: G[E] = token("type").map { _ => e.universe }

  private lazy val product: G[E] = for {
    _ <- token('(')
    x <- key
    _ <- token(':')
    y <- term
    _ <- token(')')
    _ <- token("->")
    z <- term
  } yield e.product(x, y, z)

  private lazy val parens: G[E] = for {
    _ <- token('(')
    x <- term
    _ <- token(')')
  } yield x

  private lazy val unit: G[E] = universe | identifier | let | lambda | product | parens

  lazy val term: G[E] = unit.oneOrMore.collect {
    case head :: Nil => head
    case head :: tail => e.application(head, tail)
  }

}
