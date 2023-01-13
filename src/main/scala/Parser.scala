import annotation.tailrec
import scala.util.{Try, Success, Failure}

import TokenType.*

class Parser(private var tokens: List[Token]):
  // Add null at the start for previous()
  tokens = null :: tokens

  private case class ParseError(token: Token, message: String) extends RuntimeException:
    Lox.error(token, message)

  def parse(): Expr =
    Try(expression()) match
      case Success(expr) => expr
      case Failure(error) => null


  private def expression(): Expr =
    equality()


  private def equality(): Expr =
    @tailrec def equality(expr: Expr): Expr =
      if matchAny(BANG_EQUAL, EQUAL_EQUAL) then
        val operator = previous()
        val right = comparison()
        equality(Expr.Binary(expr, operator, right))
      else expr

    equality(comparison())


  private def comparison(): Expr =
    @tailrec def comparison(expr: Expr): Expr =
      if matchAny(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL) then
        val operator = previous()
        val right = term()
        comparison(Expr.Binary(expr, operator, right))
      else expr

    comparison(term())


  private def term(): Expr =
    @tailrec def term(expr: Expr): Expr =
      if matchAny(MINUS, PLUS) then
        val operator = previous()
        val right = factor()
        term(Expr.Binary(expr, operator, right))
      else expr

    term(factor())


  private def factor(): Expr =
    @tailrec def factor(expr: Expr): Expr =
      if matchAny(SLASH, STAR) then
        val operator = previous()
        val right = unary()
        factor(Expr.Binary(expr, operator, right))
      else expr

    factor(unary())


  private def unary():Expr =
    if matchAny(BANG, MINUS) then
      val operator = previous()
      val right = unary()
      Expr.Unary(operator, right)
    else
      primary()


  private def primary(): Expr =
    if matchAny(FALSE) then Expr.Literal(false)
    else if matchAny(TRUE) then Expr.Literal(true)
    else if matchAny(NIL) then Expr.Literal(null)
    else if matchAny(NUMBER, STRING) then Expr.Literal(previous().literal)
    else if matchAny(LEFT_PAREN) then
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Expr.Grouping(expr)
    else throw ParseError(peek(), "Expect expression.")


  @tailrec
  private def matchAny(tokenTypes: TokenType*): Boolean =
    if tokenTypes.isEmpty then false
    else if check(tokenTypes.head) then
      advance()
      true
    else matchAny(tokenTypes.tail*)


  private def check(tokenType: TokenType): Boolean =
    if isAtEnd() then false
    else peek().tokenType == tokenType


  private def isAtEnd(): Boolean =
    peek().tokenType == EOF


  private def advance(): Token =
    if !isAtEnd() then tokens = tokens.tail
    previous()


  private def previous(): Token =
    tokens.head


  private def peek(): Token =
    tokens.tail.head


  private def consume(tokenType: TokenType, message: String): Token =
    if check(tokenType) then advance()
    else throw ParseError(peek(), message)


  private def synchronize(): Unit =
    advance()

    while !isAtEnd() do
      if previous().tokenType == SEMICOLON then return
      else peek().tokenType match
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => advance()

end Parser
