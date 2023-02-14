// It can't be a case class because the resolver uses a HashMap of expressions
// for scopes, and if different instances of token evaluate to the same hash for
// having the same values, the HashMap won't work properly.
class Token(
    val tokenType: TokenType,
    val lexeme: String,
    val literal: Any,
    val line: Int
):
  override def toString(): String =
    s"$tokenType $lexeme $literal"
