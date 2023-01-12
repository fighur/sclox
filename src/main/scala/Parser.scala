import TokenType.*
import scala.util.{Try, Success, Failure}

class Parser(var tokens: List[Token]):

  private case class State(previous: Token, tokens: List[Token])

  private case class ParseError(token: Token, message: String) extends RuntimeException:
    error(token, message)

  private var previous: Token = null

  def parse(): Expr =
    Try(expression()) match
      case Success(expr) => expr
      case Failure(error) => null


  private def expression(): Expr =
    equality()


  private def equality(): Expr =
    var expr = comparison()

    while matchTokenTypes(BANG_EQUAL, EQUAL_EQUAL) do
      val operator = previous
      val right = comparison()
      expr = Expr.Binary(expr, operator, right)

    expr


  private def comparison(): Expr =
    var expr = term()

    while matchTokenTypes(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) do
      val operator = previous
      val right = term()
      expr = Expr.Binary(expr, operator, right)

    expr


  private def term(): Expr =
    var expr = factor()

    while matchTokenTypes(MINUS, PLUS) do
      val operator = previous
      val right = factor()
      expr = Expr.Binary(expr, operator, right)

    expr


  private def factor():Expr =
    var expr = unary()

    while matchTokenTypes(SLASH, STAR) do
      val operator = previous
      val right = unary()
      expr = Expr.Binary(expr, operator, right)

    expr


  private def unary():Expr =
    if matchTokenTypes(BANG, MINUS) then
      val operator = previous
      val right = unary()
      Expr.Unary(operator, right)
    else
      primary()


  private def primary(): Expr =
    if matchTokenTypes(FALSE) then Expr.Literal(false.asInstanceOf[Object])
    else if matchTokenTypes(TRUE) then Expr.Literal(true.asInstanceOf[Object])
    else if matchTokenTypes(NIL) then Expr.Literal(null)
    else if matchTokenTypes(NUMBER, STRING) then Expr.Literal(previous.literal)
    else if matchTokenTypes(LEFT_PAREN) then
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Expr.Grouping(expr)
    else throw ParseError(peek(), "Expect expression.")


  private def matchTokenTypes(tokenTypes: TokenType*): Boolean =
    for tokenType <- tokenTypes do
      if check(tokenType) then
        advance()
        return true
    false


  private def check(tokenType: TokenType): Boolean =
    if isAtEnd() then false
    else peek().tokenType == tokenType


  private def isAtEnd(): Boolean =
    peek().tokenType == EOF


  private def advance(): Token =
    if !isAtEnd() then
      previous = tokens.head
      tokens = tokens.tail
    previous


  private def peek(): Token =
    tokens.head


  private def consume(tokenType: TokenType, message: String): Token =
    if check(tokenType) then advance()
    else throw ParseError(peek(), message)


  private def synchronize(): Unit =
    advance()

    while !isAtEnd() do
      if previous.tokenType == SEMICOLON then return
      else peek().tokenType match
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => advance()
