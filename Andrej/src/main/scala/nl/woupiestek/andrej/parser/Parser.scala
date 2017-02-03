package nl.woupiestek.andrej.parser

import nl.woupiestek.andrej.Expr

class Parser[E](e: Expr[E]) {

  type G[X] = Grammar[Option[Char], X]

  def key: G[String] = for {
    h <- Grammar.collect[Option[Char], Char] { case Some(c) if Character.isJavaIdentifierStart(c) => c }
    t <- Grammar.collect[Option[Char], Char] { case Some(c) if Character.isJavaIdentifierPart(c) => c }.zeroOrMore
    _ <- space
  } yield (h :: t).mkString

  private def space = Grammar.collect[Option[Char], Unit] { case Some(c) if Character.isWhitespace(c) => () }.zeroOrMore

  def identifier: G[E] = key.map(e.identifier)

  def term: G[E] = identifier | let | application | lambda | universe | product | parens

  def let: G[E] = for {
    "let" <- key
    x <- key
    '=' <- symbol
    y <- term
    ';' <- symbol
    z <- term
  } yield e.let(x, y, z)

  def application: G[E] = for {
    x <- term
    y <- term.oneOrMore
  } yield y.foldLeft(x)(e.application)

  def lambda: G[E] = for {
    '\\' <- symbol
    x <- key
    ':' <- symbol
    y <- term
    '.' <- symbol
    z <- term
  } yield e.lambda(x, y, z)

  private def symbol = for {
    x <- Grammar.collect[Option[Char], Char] { case Some(c) => c }
    _ <- space
  } yield x

  def universe: G[E] = key.collect { case x if "type" == x => e.universe }

  def product: G[E] = for {
    "product" <- key
    x <- key
    ':' <- symbol
    y <- term
    '.' <- symbol
    z <- term
  } yield e.product(x, y, z)

  def parens: G[E] = for {
    '(' <- symbol
    x <- term
    ')' <- symbol
  } yield x

}
