package nl.woupiestek.andrej.parse

sealed trait Parse[T] {
  def flatMap[U](f: T => Parse[U]): Parse[U]
}

object Parse {

  case class Return[T](value: T) extends Parse[T] {
    override def flatMap[U](f: (T) => Parse[U]): Parse[U] = f(value)
  }

  case class Read[T](next: Option[Char] => List[Parse[T]]) extends Parse[T] {
    override def flatMap[U](f: (T) => Parse[U]): Parse[U] = Read {
      oc => next(oc).map(_.flatMap(f))
    }
  }

}

object Lisp {

  sealed trait AST

  case class Atom(name: String) extends AST

  case class Tree(subs: List[AST]) extends AST

  import Parse._

  def sParse: Parse[AST] = Read {
    case Some(char) if Character.isWhitespace(char) => List(sParse)
    case Some('(') => List(lParse(Nil))
    case Some(char) if Character.isJavaIdentifierPart(char) => List(aParse(List(char)))
    case None => List(Return(Tree(Nil)))
  }

  def lParse(asts: List[AST]): Parse[AST] = Read {
    case None => Nil
    case Some(')') => List(Return(Tree(asts.reverse)))
    case Some(ws) if Character.isWhitespace(ws) => List(lParse(asts))
    case Some(ch) if Character.isJavaIdentifierStart(ch) => List(aParse(List(ch)).flatMap(ast => lParse(ast :: asts)))
  }

  def aParse(chars: List[Char]): Parse[AST] = {
    def end: List[Parse[AST]] = List(Return(Atom(chars.reverse.mkString)))
    Read {
      case None => end
      case Some(')') => end
      case Some(ws) if Character.isWhitespace(ws) => end
      case Some(ch) if Character.isJavaIdentifierPart(ch) => List(aParse(ch :: chars))
    }
  }
}
