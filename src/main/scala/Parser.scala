
import annotation.tailrec
import scala.util.{Try, Success, Failure}

import TokenType.*

class Parser(private var tokens: List[Token]):
  // Add null at the start for previous()
  tokens = null :: tokens

  private class ParseError extends RuntimeException

  def parse(): List[Stmt] =
    @tailrec def parse(stmts: List[Stmt]): List[Stmt] =
      if isAtEnd() then stmts
      else parse(declaration() :: stmts)

    parse(Nil).reverse


  private def declaration(): Stmt =
    Try(if matchAny(VAR) then varDeclaration() else statement()) match
      case Success(stmt) => stmt
      case Failure(pe: ParseError) =>
        synchronize()
        null
      case _ => ???


  private def varDeclaration(): Stmt =
    val name = consume(IDENTIFIER, "Expect variable name.")
    val initializer =  if matchAny(EQUAL) then expression() else null

    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Stmt.Var(name, initializer)


  private def statement(): Stmt =
    if matchAny(PRINT) then printStatement()
    else if matchAny(LEFT_BRACE) then Stmt.Block(block())
    else expressionStatement()


  private def printStatement(): Stmt =
    val value = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Stmt.Print(value)


  private def block(): List[Stmt] =
    @tailrec def block(stmts: List[Stmt]): List[Stmt] =
      if check(RIGHT_BRACE) || isAtEnd() then
        consume(RIGHT_BRACE, "Expect '}' after block.")
        stmts.reverse
      else
        block(declaration() :: stmts)

    block(Nil)


  private def expressionStatement(): Stmt =
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Stmt.Expression(expr)


  private def expression(): Expr =
    assignment()


  private def assignment(): Expr =
    equality() match
      case Expr.Variable(name) if matchAny(EQUAL) =>
        val value = assignment()
        Expr.Assign(name, value)
      case expr if matchAny(EQUAL) =>
        val equals = previous()
        assignment()
        error(equals, "Invalid assignment target.")
        expr
      case expr => expr


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
    else if matchAny(IDENTIFIER) then Expr.Variable(previous())
    else if matchAny(LEFT_PAREN) then
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      Expr.Grouping(expr)
    else throw error(peek(), "Expect expression.")


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
    else throw error(peek(), message)


  private def error(token: Token, message: String): ParseError =
    Lox.error(token, message)
    ParseError()

  private def synchronize(): Unit =
    advance()

    @tailrec def synch(): Unit =
      if !isAtEnd() then
        if previous().tokenType == SEMICOLON then ()
        else peek().tokenType match
          case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => ()
          case _ =>
            advance()
            synch()

    synch()

end Parser
