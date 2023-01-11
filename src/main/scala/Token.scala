case class Token(tokenType: TokenType, lexeme: String, literal: Object, line: Int):
  override def toString(): String =
    s"$tokenType $lexeme $literal"