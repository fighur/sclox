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
    lazy val matchedStmt =
      if matchAny(FUN) then function("function")
      else if matchAny(CLASS) then classDeclaration()
      else if matchAny(VAR) then varDeclaration()
      else statement()

    Try(matchedStmt) match
      case Success(stmt) => stmt
      case Failure(pe: ParseError) =>
        synchronize()
        null
      case _ => ??? // Unreachable

  private def classDeclaration(): Stmt =
    @tailrec def parseMethods(
        methods: List[Stmt.Function]
    ): List[Stmt.Function] =
      if check(RIGHT_BRACE) || isAtEnd() then methods.reverse
      else parseMethods(function("method") :: methods)

    val name = consume(IDENTIFIER, "Expect class name.")
    val superclass: Option[Expr.Variable] =
      if matchAny(LESS) then
        consume(IDENTIFIER, "Expect superclass name.")
        Some(Expr.Variable(previous()))
      else None

    consume(LEFT_BRACE, "Expect '{' before class body.")
    val methods = parseMethods(Nil)
    consume(RIGHT_BRACE, "Expect '}' after class body.")

    Stmt.Class(name, superclass, methods)

  private def function(kind: String): Stmt.Function =
    @tailrec def parseParams(params: List[Token], count: Int): List[Token] =
      if matchAny(COMMA) then
        if (count >= 255) then
          error(peek(), "Can't have more than 255 parameters.")
        parseParams(
          consume(IDENTIFIER, "Expect parameter name") :: params,
          count + 1
        )
      else params.reverse

    val name = consume(IDENTIFIER, s"Expect $kind name.")
    consume(LEFT_PAREN, s"Expect '(' after $kind name.")

    val parameters =
      if !check(RIGHT_PAREN) then
        parseParams(consume(IDENTIFIER, "Expect parameter name") :: Nil, 1)
      else Nil

    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    consume(LEFT_BRACE, s"Expect '{' before $kind body.")

    val body = block()

    Stmt.Function(name, parameters, body)

  private def varDeclaration(): Stmt =
    val name = consume(IDENTIFIER, "Expect variable name.")
    val initializer = if matchAny(EQUAL) then Some(expression()) else None

    consume(SEMICOLON, "Expect ';' after variable declaration.")
    Stmt.Var(name, initializer)

  private def statement(): Stmt =
    if matchAny(FOR) then forStatement()
    else if matchAny(IF) then ifStatement()
    else if matchAny(PRINT) then printStatement()
    else if matchAny(RETURN) then returnStatement()
    else if matchAny(WHILE) then whileStatement()
    else if matchAny(LEFT_BRACE) then Stmt.Block(block())
    else expressionStatement()

  private def forStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    val initializer =
      if matchAny(SEMICOLON) then null
      else if matchAny(VAR) then varDeclaration()
      else expressionStatement()

    var condition =
      if !check(SEMICOLON) then expression() else Expr.Literal(true)
    consume(SEMICOLON, "Expect ';' after loop condition.")

    val increment = if !check(RIGHT_PAREN) then expression() else null
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")

    val body =
      if increment != null then
        Stmt.Block(List(statement(), Stmt.Expression(increment)))
      else statement()

    val loop = Stmt.While(condition, body)

    val forLoop =
      if initializer != null then Stmt.Block(List(initializer, loop))
      else loop

    forLoop

  private def ifStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")
    val thenBranch = statement()
    val elseBranch = if matchAny(ELSE) then Some(statement()) else None

    Stmt.If(condition, thenBranch, elseBranch)

  private def printStatement(): Stmt =
    val value = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    Stmt.Print(value)

  private def returnStatement(): Stmt =
    val keyword = previous()
    var value = if !check(SEMICOLON) then Some(expression()) else None

    consume(SEMICOLON, "Expect ';' after return value.")
    Stmt.Return(keyword, value)

  private def whileStatement(): Stmt =
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after condition.")
    val body = statement()

    Stmt.While(condition, body)

  private def block(): List[Stmt] =
    @tailrec def block(stmts: List[Stmt]): List[Stmt] =
      if check(RIGHT_BRACE) || isAtEnd() then
        consume(RIGHT_BRACE, "Expect '}' after block.")
        stmts.reverse
      else block(declaration() :: stmts)

    block(Nil)

  private def expressionStatement(): Stmt =
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after expression.")
    Stmt.Expression(expr)

  private def expression(): Expr =
    assignment()

  private def assignment(): Expr =
    or() match
      case Expr.Variable(name) if matchAny(EQUAL) =>
        val value = assignment()
        Expr.Assign(name, value)
      case Expr.Get(instance, name) if matchAny(EQUAL) =>
        val value = assignment()
        Expr.Set(instance, name, value)
      case expr if matchAny(EQUAL) =>
        val equals = previous()
        assignment()
        error(equals, "Invalid assignment target.")
        expr
      case expr => expr

  private def or(): Expr =
    @tailrec def or(expr: Expr): Expr =
      if matchAny(OR) then
        val operator = previous()
        val right = and()
        or(Expr.Logical(expr, operator, right))
      else expr

    or(and())

  private def and(): Expr =
    @tailrec def and(expr: Expr): Expr =
      if matchAny(AND) then
        val operator = previous()
        val right = equality()
        and(Expr.Logical(expr, operator, right))
      else expr

    and(equality())

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

  private def unary(): Expr =
    if matchAny(BANG, MINUS) then
      val operator = previous()
      val right = unary()
      Expr.Unary(operator, right)
    else call()

  private def call(): Expr =
    @tailrec def call(expr: Expr): Expr =
      if matchAny(LEFT_PAREN) then call(finishCall(expr))
      else if matchAny(DOT) then
        val name = consume(IDENTIFIER, "Expect property name after '.'.")
        call(Expr.Get(expr, name))
      else expr

    call(primary())

  private def finishCall(callee: Expr): Expr =
    @tailrec def parseArgs(args: List[Expr], count: Int): List[Expr] =
      if matchAny(COMMA) then
        if (count >= 255) then
          error(peek(), "Can't have more than 255 arguments.")
        parseArgs(expression() :: args, count + 1)
      else args.reverse

    val arguments =
      if !check(RIGHT_PAREN) then parseArgs(expression() :: Nil, 1)
      else Nil
    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    Expr.Call(callee, paren, arguments)

  private def primary(): Expr =
    if matchAny(FALSE) then Expr.Literal(false)
    else if matchAny(TRUE) then Expr.Literal(true)
    else if matchAny(NIL) then Expr.Literal(null)
    else if matchAny(NUMBER, STRING) then Expr.Literal(previous().literal)
    else if matchAny(THIS) then Expr.This(previous())
    else if matchAny(SUPER) then
      val keyword = previous()
      consume(DOT, "Expect '.' after 'super'.")
      val method = consume(IDENTIFIER, "Expect superclass method name.")
      Expr.Super(keyword, method)
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
        else
          peek().tokenType match
            case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => ()
            case _ =>
              advance()
              synch()

    synch()

end Parser
