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
  println(tokens.mkString("\n"))


def error(line: Int, message: String): Unit =
  report(line, "", message)


def report(line: Int, where: String, message: String): Unit =
  Console.err.println(s"[line ${line}] Error ${where}: ${message}")
  hadError = true