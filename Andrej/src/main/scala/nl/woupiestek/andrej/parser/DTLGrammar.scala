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

  private lazy val let: G[E] = for {
    "let" <- key
    x <- key
    "be" <- key
    y <- term
    "in" <- key
    z <- term
  } yield e.let(x, y, z)

  private lazy val symbol = for {
    x <- Rule.collect[Option[Char], Char] { case Some(c) => c }
    _ <- space
  } yield x

  private lazy val lambda: G[E] = for {
    '\\' <- symbol
    x <- key
    ':' <- symbol
    y <- term
    '.' <- symbol
    z <- term
  } yield e.lambda(x, y, z)

  private lazy val universe: G[E] = key.collect { case x if "type" == x => e.universe }

  private lazy val product: G[E] = for {
    "pi" <- key
    x <- key
    ':' <- symbol
    y <- term
    '.' <- symbol
    z <- term
  } yield e.product(x, y, z)

  private lazy val parens: G[E] = for {
    '(' <- symbol
    x <- term
    ')' <- symbol
  } yield x

  private lazy val unit: G[E] = universe | identifier | let | lambda | product | parens

  lazy val term: G[E] = unit.oneOrMore.collect {
    case head :: Nil => head
    case head :: tail => tail.foldLeft(head)(e.application)
  }

}
