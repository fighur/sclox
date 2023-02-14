import scala.io.{Source, StdIn}
import annotation.tailrec

object Lox:
  private var hadError = false
  private var hadRuntimeError = false
  private val interpreter = Interpreter()

  @main def sclox(args: String*): Unit =
    if args.length > 1 then
      println("Usage sclox [script]")
      sys.exit(64)
    else if args.length == 1 then Lox.runFile(args.head)
    else Lox.runPrompt()

  def runFile(path: String): Unit =
    run(Source.fromFile(path).mkString)
    if hadError then System.exit(65)
    if hadRuntimeError then System.exit(70)

  @tailrec
  def runPrompt(): Unit =
    print("> ")
    val line = StdIn.readLine()
    if line == null then { /* Stop */ }
    else
      run(line)
      hadError = false
      runPrompt()

  def run(source: String): Unit =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()
    val parser = Parser(tokens)
    val statements = parser.parse()

    if hadError then ()
    else
      val resolver = Resolver(interpreter)
      resolver.resolve(statements)
      if hadError then ()
      else interpreter.interpret(statements)

  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def error(token: Token, message: String): Unit =
    if token.tokenType == TokenType.EOF then
      report(token.line, " at end", message)
    else report(token.line, s" at '${token.lexeme}'", message)

  def report(line: Int, where: String, message: String): Unit =
    Console.err.println(s"[line ${line}] Error${where}: ${message}")
    hadError = true

  def runtimeError(error: RuntimeError): Unit =
    Console.err.println(s"${error.message}\n[line ${error.token.line}]")
    hadRuntimeError = true
