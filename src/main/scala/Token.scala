case class Token(tokenType: TokenType, lexeme: String, literal: Any, line: Int):
  override def toString(): String =
    s"$tokenType $lexeme $literal"