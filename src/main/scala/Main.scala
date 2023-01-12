import scala.io.{Source, StdIn}
import annotation.tailrec

var hadError = false
@main def lox(args: String*): Unit =
  if args.length > 1 then
    println("Usage sclox [script]")
    sys.exit(64)
  else if args.length == 1 then
    runFile(args.head)
  else
    runPrompt()


def runFile(path: String): Unit =
  run(Source.fromFile(path).mkString)
  if hadError then System.exit(65)


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
  val expression = parser.parse()

  if (hadError) return

  println(AstPrinter().print(expression))


def error(line: Int, message: String): Unit =
  report(line, "", message)


def error(token: Token, message: String): Unit =
  if token.tokenType == TokenType.EOF then
    report(token.line, " at end", message)
  else
    report(token.line, s" at '${token.lexeme}'", message)


def report(line: Int, where: String, message: String): Unit =
  Console.err.println(s"[line ${line}] Error ${where}: ${message}")
  hadError = true