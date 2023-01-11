import annotation.tailrec
import TokenType.*

class Scanner(source: String):
  private var start = 0
  private var current = 0
  private var line = 1

  private val keywords = Map(
    "and"    -> AND,
    "class"  -> CLASS,
    "else"   -> ELSE,
    "false"  -> FALSE,
    "for"    -> FOR,
    "fun"    -> FUN,
    "if"     -> IF,
    "nil"    -> NIL,
    "or"     -> OR,
    "print"  -> PRINT,
    "return" -> RETURN,
    "super"  -> SUPER,
    "this"   -> THIS,
    "true"   -> TRUE,
    "var"    -> VAR,
    "while"  -> WHILE,
  )

  def scanTokens(): List[Token] =
    scanTokens(Nil)


  @tailrec
  private def scanTokens(tokens: List[Token]): List[Token] =
    scanToken() match
      case Right(eof @ Token(EOF, _, _, _)) => (eof :: tokens).reverse
      case Right(token) => scanTokens(token :: tokens)
      case Left(errorMessage) =>
        error(line, errorMessage)
        scanTokens(tokens)


  @tailrec
  private def scanToken(): Either[String, Token] =
    start = current
    if !isAtEnd() then advance() match
      case '(' => buildToken(LEFT_PAREN)
      case ')' => buildToken(RIGHT_PAREN)
      case '{' => buildToken(LEFT_BRACE)
      case '}' => buildToken(RIGHT_BRACE)
      case ',' => buildToken(COMMA)
      case '.' => buildToken(DOT)
      case '-' => buildToken(MINUS)
      case '+' => buildToken(PLUS)
      case ';' => buildToken(SEMICOLON)
      case '*' => buildToken(STAR)
      case '!' =>
        if matchNext('=') then buildToken(BANG_EQUAL)
        else buildToken(BANG)
      case '=' =>
        if matchNext('=') then buildToken(EQUAL_EQUAL)
        else buildToken(EQUAL)
      case '<' =>
        if matchNext('=') then buildToken(LESS_EQUAL)
        else buildToken(LESS)
      case '>' =>
        if matchNext('=') then buildToken(GREATER_EQUAL)
        else buildToken(GREATER)
      case '/' =>
        if matchNext('/') then
          while peek() != '\n' && !isAtEnd() do advance()
          scanToken()
        else buildToken(SLASH)
      case ' ' | '\r' | '\t' =>
        scanToken()
      case '\n' =>
        line += 1
        scanToken()
      case '"' => string()
      case c if isDigit(c) => number()
      case c if isAlpha(c) => identifier()
      case _   => buildToken(null)
    else Right(Token(EOF, "", null, line))
  end scanToken


  private def string(): Either[String, Token] =
    while peek() != '"' && !isAtEnd() do
      if peek() == '\n' then line += 1
      advance()

    if isAtEnd() then Left("Unterminated string.")
    else
      advance()
      val value = source.substring(start + 1, current - 1)
      Right(buildToken(STRING, value))


  private def number(): Either[String, Token] =
    while isDigit(peek()) do advance()

    if peek() == '.' && isDigit(peekNext()) then
      advance()
      while isDigit(peek()) do advance()

    val num = source.substring(start, current).toDouble.asInstanceOf[Object]
    Right(buildToken(NUMBER, num))


  private def identifier(): Either[String, Token] =
    while isAlphaNumberic(peek()) do advance()

    val text = source.substring(start, current)
    keywords.get(text) match
      case Some(tokenType) => buildToken(tokenType)
      case None => buildToken(IDENTIFIER)


  private def isAtEnd(): Boolean =
    current >= source.length


  private def advance(): Char =
    val c = source(current)
    current += 1
    c


  private def peek(): Char =
    if isAtEnd() then Char.MinValue
    else source(current)


  private def peekNext(): Char =
    if current + 1 >= source.length then Char.MinValue
    else source(current + 1)


  private def matchNext(expected: Char): Boolean =
    if isAtEnd() then false
    else if source(current) != expected then false
    else
      current += 1
      true


  private def isAlphaNumberic(c: Char): Boolean = isAlpha(c) || isDigit(c)


  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'


  private def isAlpha(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_')


  private def buildToken(tokenType: TokenType): Either[String, Token] =
    if tokenType == null then Left("Unexpected character.")
    else Right(buildToken(tokenType, null))


  private def buildToken(tokenType: TokenType, literal: Object): Token =
    val text = source.substring(start, current)
    Token(tokenType, text, literal, line)